#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file.
#' Once the instance is created, the model can either be fed to a \code{\link{libbi}}
#' object.
#'
#' @param filename is the file name of the model file
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' @seealso \code{\link{fix}}, \code{\link{propose_prior}}, \code{\link{get_lines}}, \code{\link{insert_lines}}, \code{\link{update_lines}}, \code{\link{remove_lines}}, \code{\link{rename}}, \code{\link{write_file}}, \code{\link{clone}}
#' @export bi_model
bi_model <- function(filename, lines) {
  if (!missing(filename) && !missing(lines)) {
    stop("Only one of 'filename' or 'lines' can be given")
  }
  if (!missing(filename)) {
    if (length(as.character(filename)) == 0) {
      stop ("Filename must be a non-empty character string")
    }
  }

  if (!missing(filename)) {
    model <- readLines(filename)
  } else if (!missing(lines)) {
    model <- lines
  } else {
    model <- character(0)
  }

  new_obj <- structure(list(model=model,
                            name=""), class="bi_model")

  return(clean_model(new_obj))
}

fix <- function(x, ...) UseMethod("fix")
#' @rdname fix
#' @name fix
#' @title Fix noise term, state or parameter of a libbi model
#' @description
#' Replaces all variables with fixed values as given ; note that this will not replace differential equations and lead to an error if applied to states that are changed inside an "ode" block
#'
#' @param x a \code{\link{bi_model}} object
#' @param ... values to be assigned to the (named) variables
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' fix(PZ, alpha = 0)
#' @export
fix.bi_model <- function(x, ...) {
  "Fix a parameter, variable or noise term in the model"

  fix_model <- x$model

  fixed <- list(...)

  ## variables that are to be fixed
  var_str <-
    paste0("^[[:space:]]*(noise|param|state|input|const)[[:space:]]+(",
           paste(names(fixed), collapse = "|"), ")([[:space:][]|$)")
  var_line_nbs <- grep(var_str, fix_model)

  var_vec <- c(x$get_vars("noise"),
               x$get_vars("param"),
               x$get_vars("state"),
               x$get_vars("input"),
               x$get_vars("const"))

  unmatched_names <- setdiff(names(fixed), var_vec)

  if (length(var_line_nbs) > 0)
  {
    fix_model <- fix_model[-var_line_nbs]
  }

  for (name in unmatched_names)
  {
    fixed_line <-
      paste0("const ", name, " = ", fixed[[name]])
    if (length(var_line_nbs) > 0)
    {
      first_const_line <- var_line_nbs[1]
    } else
    {
      first_const_line <- grep("^[[:space:]]*(noise|param|state|input|const)[[:space:]]+", fix_model)[1]
    }
    fix_model <-
      c(fix_model[1:(first_const_line - 1)],
        fixed_line,
        fix_model[first_const_line:length(fix_model)])
  }

  for (var in intersect(names(fixed), var_vec)) {
    ## remove assignments
    assignments <-
      grep(paste0(var,
                  "(/dt)?[[:space:]]*(\\[[^]]*\\])?[[:space:]]*(~|=|<-)"),
           fix_model)
    if (length(assignments) > 0) {
      fix_model <- fix_model[-assignments]
    }

    ## remove dimensions
    fix_model <-
      gsub(paste0(var, "[[:space:]]*\\[[^]]*\\]"), var, fix_model)

    ## add const assignment
    fixed_line <- paste0("const ", var, " = ", fixed[[var]])
    fix_model <- c(fix_model[1:(var_line_nbs[1] - 1)],
                   fixed_line, 
                   fix_model[(var_line_nbs[1]):length(fix_model)])

  }

  x$model <- fix_model
  return(clean_model(x))
}

propose_prior <- function(x, ...) UseMethod("propose_prior")
#' @rdname propose_prior
#' @name propose_prior
#' @title Propose from the prior in a libbi model
#' @description
#' Generates a version of the model where the proposal blocks are replaced by the prior blocks. This is useful for exploration of the likelihood surface.
#'
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' propose_prior(PZ)
propose_prior.bi_model <- function(x) {
          "Change a libbi model to make the prior the proposal distribution"
          new_model <- bi_model(lines = x$model)

          ## remove parameter proposal
          prior_parameter <- new_model$get_block("parameter")
          new_model$add_block("proposal_parameter", lines = prior_parameter)

          prior_initial <- new_model$get_block("initial")
          new_model$add_block("proposal_initial", lines = prior_initial)

          return(new_model)
}

#' @title Copy obs variables to state variables (with '__sample_' prepended)
#'
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
obs_to_noise <- function(x) {
          new_model <- bi_model(lines = x$model)
          obs_block <- get_block("observation")
          obs_variables <- get_vars("obs")

          obs_var_pattern <- paste0("^(", paste(obs_variables, collapse = "|"), ")")
          state_block <- sub(obs_var_pattern, "__sample_\\1", obs_block)
          new_model$insert_lines(state_block, at_end_of = "transition")
          state_variables <- paste0("__sample_", obs_variables)
          new_model$insert_lines(paste("noise ", paste(state_variables, collapse = ", ")), after = 1)

          return(new_model)
        }

#' @title Strip model code to its bare bones
#'
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
clean_model <- function(x) {
    ## strip comments
    x$model <- sub("//.*$", "", x$model)

    ## remove long comments and merge lines
    i <- 1
    comment <- FALSE
    while (i <= length(x$model)) {
        x$model[i] <- gsub("/\\*[^(\\*)]*\\*/", "", x$model[i])
        if (grepl("/\\*", x$model[i])) {
            x$model[i] <- sub("/\\*.*$", "", x$model[i])
            comment <- TRUE
        }
        if (grepl("\\*/", x$model[i])) {
            x$model[i] <- sub("^.*\\*/", "", x$model[i])
            comment <- FALSE
        }

        if (comment) {
            x$model <- x$model[-i]
        } else {
            if (!comment && grepl("[*-+=/][[:space:]]*$", x$model[i])) {
                x$model[i] <- paste(x$model[i], x$model[i + 1])
                x$model <- x$model[-(i + 1)]
            } else {
                i <- i + 1
            }
        }
    }

    ## remove multiple spaces
    x$model <- gsub("[[:space:]]+", " ", x$model)
    ## make sure there is a line break after opening braces 
    x$model <- gsub("\\{(.+)$", "{\n\\1", x$model)
    ## make sure there is a line break before closing braces
    x$model <- gsub("^(.+)\\}", "\\1\n}", x$model)
    ## replace semicolons with newlines
    x$model <- gsub(";", "\n", x$model)
    ## remove trailing spaces
    x$model <- gsub("[[:space:]]*$", "", x$model)
    ## remove initial spaces
    x$model <- gsub("^[[:space:]]*", "", x$model)
    ## remove empty lines
    x$model <- x$model[x$model!=""]

    if (length(x$model) > 0) {
        x$name <- gsub("model ", "", gsub("\\{", "", x$model[1]))
        x$name <- gsub("[[:space:]]*$", "", x$name)
        x$name <- gsub("^[[:space:]]*", "", x$name)
    } else {
        x$name <- character(0)
    }
    return(x)
}

#' @rdname get_lines
#' @name get_lines
#' @title Get lines in a libbi model
#' @description
#' Gets a libbi model as character vector.
#'
#' @param x a \code{\link{bi_model}} object
#' @param spaces Number of spaces of indentation
#' @return the libbi model as character vector
#' @seealso \code{\link{bi_model}}, \code{\link{`[`}}
#' @keywords internal
get_lines <- function(x, spaces = 2) {
    "Get the lines of a libbi model as character vectore"
    lines <- c()
    indent <- 0
    for (i in seq_along(x$model)) {
        if (grepl("\\}", x$model[i])) {
            if (indent > 0) {
                indent <- indent - 1
            } else {
                warning("There seems to be a pair of unbalanced braces")
            }
        }
        indent_spaces <- paste(rep(" ", indent * spaces), collapse = "")
        lines <- c(lines, paste0(indent_spaces, x$model[i]))
        if (grepl("\\{", x$model[i])) {
            indent <- indent + 1
        }
    }
    return(lines)
}

insert <- function(x, ...) UseMethod("insert")
#' @rdname insert
#' @name insert
#' @title Insert lines in a LibBi model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or \code{after} is given, the line(s) will be inserted before or after a given line number or block name, respectively. If one of \code{at_beginning of} or \code{at_end_of} is given, the lines will be inserted at the beginning/end of the block, respectively
#'
#' @param x a \code{\link{bi_model}} object
#' @param lines vector or line(s)
#' @param before line number before which to insert line(s)
#' @param after line number after which to insert line(s)
#' @param at_beginning_of block at the beginning of which to insert lines(s)
#' @param at_end_of block at the end of which to insert lines(s)
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- insert_lines(PZ, lines = "noise beta", after = 8)
insert.bi_model <- function(x, lines, before, after, at_beginning_of, at_end_of) {
          args <- match.call()
          arg_name <- setdiff(names(args), c("", "lines"))
          if (length(arg_name) != 1) {
            stop("insert_lines needs exactly two arguments, 'lines' and one of 'before', 'after', 'at_beginning_of' or 'at_end_of'")
          }
          arg <- get(arg_name)
          if (is.numeric(arg)) arg <- as.integer(arg)

          if (arg_name %in% c("before", "after") && is.integer(arg)) {
            if (arg_name == "before") {
              after <- before - 1
            }
            if (after > length(model)) {
              stop("model only has ", length(model), " lines, higher requested.")
            }
          } else {
            block_lines <- x$find_block(arg)
            if (length(block_lines) > 0) {
              if (arg_name == "before") {
                after <- block_lines[1] - 1
              } else if (arg_name == "after") {
                after <- block_lines[length(block_lines)]
              } else if (arg_name == "at_beginning_of") {
                after <- block_lines[1]
              } else if (arg_name == "at_end_of") {
                after <- block_lines[length(block_lines)] - 1
              } else {
                stop("Unknown argument: ", arg_name)
              }
            } else {
              stop("Could not find block ", arg)
            }
          }

          if (after == 0) {
            model <<- c(lines, model)
          } else if (after == length(model)) {
            model <<- c(model[1:after], lines)
          } else {
            model <<- c(model[1:after], lines, model[(after+1):length(model)])
          }
          clean_model()
        }


#' @rdname replace_lines
#' @name replace_lines
#' @title Update line(s) in a libbi model
#' @description
#' Updates one or more lines in a libbi model.
#'
#' @param num line number(s) to update
#' @param lines vector of line(s)
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}, \code{\link{`[<-`}}
#' @keywords internal
replace_lines <- function(x, num, lines) {
  x$model[num] <- lines
  return(clean_model(x))
}

remove <- function(x, ...) UseMethod("remove")
#' @rdname remove
#' @name remove
#' @title Remove line(s) in a libbi model
#' @description
#' Removes one or more lines in a libbi model.
#'
#' @param num line number(s) to remove
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- remove_lines(PZ, 2)
#' @export
remove.bi_model <- function(x, num) {
  if (length(num) > 0) {
    if (length(grep("[\\{\\}]", x$model[num])) %% 2 == 1) {
      stop("Removing lines would create unbalanced braces.")
    }
    x$model <- x$model[-num]
    clean_model()
  }
}

rename <- function(x, ...) UseMethod("rename")

#' @rdname rename
#' @name rename
#' @title Set the name of a bi model
#' @description
#' Changes the name of a bi model (first line of the .bi file) to the specified name.
#'
#' @param x a \code{\link{bi_model}} object
#' @param name Name of the model
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- set_name(PZ, "new_PZ")
#' @export
rename.bi_model <- function(x, name) {
          if (length(model) > 0) {
            if (grepl("model [[:graph:]]+ \\{", model[1])) {
              model[1] <<-
                sub("model [[:graph:]]+ \\{", paste0("model ", name, " {"),
                    model[1])
            } else {
              stop("could not identify model name in first line")
            }
          } else {
            model <<- c(paste0("model ", name, " {"),
                        "}")
          }
          return(clean_model(x))
        }

write_model_file <- function(x, ...) UseMethod("write_model_file")
write_model_file.bi_model <- function(x, filename) {
  stop("'write_model_file' is deprecated and will be removed in version 0.7. Use 'write'.")
  x$write(filename)
}

write_file <- function(x, ...) UseMethod("write")
#' @rdname write_file
#' @name write_file
#' @title Writes a bi model to a file.
#' @description
#' Writes a bi model to a file given by \code{filename}. The extension '.bi' will be added if necessary.
#'
#' @param filename name of the file to be written
#' @return the return value of the \code{\link{writeLines}} call.
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$write("PZ")
write_file.bi_model <- function(x, filename) {
  "Write model to file"
  if (!grepl("\\.bi$", filename)) {
    filename <- paste(filename, "bi", sep = ".")
  }
  model_name <- sub("\\.bi$", "", basename(filename))

  writeLines(get_lines(x), con = filename, sep = "\n")
}

find_block <- function(x, ...) UseMethod("find_block")
find_block.bi_model <- function(x, name) {
  lines <- x$model
  sub_regexp <- paste0("^[[:space:]]*(sub[[:space:]]+)?[[:space:]]*", name, "[[:space:]]*(\\(.*\\))?[[:space:]]*\\{")
  sub_line <- grep(sub_regexp, lines)
  if (length(sub_line) == 1) {
    lines[sub_line] <- sub(sub_regexp, "", lines[sub_line])
    open_braces <- 1
    line <- sub_line - 1
    while(open_braces > 0) {
      line <- line + 1
      braces <- grep("[\\{\\}]", lines[line], value = TRUE)
      open_braces <- open_braces + nchar(gsub("\\}", "", lines[line])) - nchar(gsub("\\{", "", lines[line]))
    }
    return(sub_line:line)
  } else {
    return(integer(0))
  }
}

get_block <- function(x, ...) UseMethod("get_block")
get_block.bi_model <- function(x, name) {
  block <- x$find_block(name)
  if (length(block) > 0) {
    lines <- x$model[block]
    lines[1] <-
      sub(paste0("^[[:space:]]*(sub[[:space:]]+)?", name, "[[:space:]]*\\{"), "", lines[1])
    lines[length(lines)] <- sub("\\}[[:space:]]*$", "", lines[length(lines)])
    empty_lines <- grep("^[[:space:]]*$", lines)
    if (length(empty_lines) > 0) {
      lines <- lines[-empty_lines]
    }
    return(lines)
  } else {
    return(NA)
  }
}

remove_block <- function(x, ...) UseMethod("remove_block")
remove_block.bi_model <- function(x, name) {
  block <- find_block(name)
  if (length(block) > 0) {
    model <<- x$model[-block]
  }
}

add_block <- function(x, ...) UseMethod("add_block")
add_block.bi_model <- function(x, name, lines, options) {
  x$remove_block(name)
  model <<- c(x$model[seq_len(length(x$model) - 1)],
              ifelse(missing(options), paste("sub", name,"{"),
                     paste("sub", name, paste0("(", options, ")", "{"))), 
              paste(lines, sep = "\n"), "}", "}")
  clean_model()
}

get_vars <- function(x, ...) UseMethod("get_vars")
get_vars.bi_model <- function(x, type, dim = FALSE, opt = FALSE) {
  line_nbs <- grep(paste0("^[[:space:]]*", type, "[[:space:]]"), x$model)
  if (length(line_nbs) > 0) {
    ## remove qualifier
    names <- sub(paste0("^[[:space:]]*", type, "[[:space:]]"), "",
                 x$model[line_nbs])
    if (!dim) {
      ## remove dimensions
      names <- sub("\\[.*\\]", "", names)
    }
    if (!opt) {
      ## remove options
      names <- sub("\\(.*\\)", "", names)
    }
    if (type == "const") {
      ## remove assignments
      names <- sub("=.*$", "", names)
    }
    ## remove spaces
    names <- gsub("[[:space:]]", "", names)
    names_vec <- unlist(strsplit(names, ","))
    return(names_vec)
  } else {
    return(c())
  }
}

print.bi_model <- function(x) {
    if (!is.null(x$name)) {
    cat("bi model:", x$name, "\n")
    cat("==========", paste(rep("=", nchar(x$name)), collapse = ""), "\n",
        sep = "")
  } else {
    cat("unnamed bi model", x$name, "\n")
    cat("================\n")
  }
  lines <- get_lines(x)
  if (is.null(lines)) {
    cat("// empty", "\n")
  } else {
    print(lines)
  }
}

#' @name `[`
#' @title Subset of model lines
#' @description
#' Extracts a subset of lines from the model
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:5]
#' @export
#' @param x A bi_model
#' @param i A vector of line numbers
`[.bi_model` <- function(x, i) {
    return(get_lines(x)[i])
}

#' @name `[<-`
#' @title Subset assignment to mode lines
#' @description
#' Assigns new character strings a subset of lines from the model
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
#' @export
#' @param x A bi_model
#' @param i A vector of line numbers
#' @param value A vector of the same length as \code{i}, containing the
#'   replacement strings 
`[<-.bi_model` <- function(x, i, value) {
    x <- replace_lines(x, i, value)
    return(clean_model(x))
}
