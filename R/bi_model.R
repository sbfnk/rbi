#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file, URL or character vector.
#' Once the instance is created, the model can be fed to a \code{\link{libbi}} object.
#'
#' @param filename the file name of the model file
#' @param url the URL of the model
#' @param lines lines of the model (if no \code{filename} is given), a character vector
#' @param ... ignored
#' @importFrom curl curl
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' @seealso \code{\link{fix}}, \code{\link{insert_lines}}, \code{\link{remove_lines}}, \code{\link{replace_all}}, \code{\link{get_name}}, \code{\link{set_name}}, \code{\link{write_file}}
#' @export
bi_model <- function(filename, url, lines, ...) {
  if (sum(!missing(filename), !missing(url), !missing(lines)) > 1) {
    stop("Only one of 'filename', 'url' or 'lines' can be given")
  }
  if (!missing(filename)) {
    if (length(as.character(filename)) == 0) {
      stop ("Filename must be a non-empty character string")
    }
  }

  if (!missing(url)) {
    if (length(as.character(url)) == 0) {
      stop ("Filename must be a non-empty character string")
    }
  }

  if (!missing(filename)) {
    model <- readLines(filename)
  } else if (!missing(url)) {
    con <- curl(url)
    model <- readLines(con)
    close(con)
  } else if (!missing(lines)) {
    model <- lines
  } else {
    model <- character(0)
  }

  return(clean_model(model))
}

#' @export
fix <- function(x, ...) UseMethod("fix")
#' @rdname fix
#' @name fix
#' @title Fix noise term, state or parameter of a libbi model
#' @description
#' Replaces all variables with fixed values as given ; note that this will not replace differential equations and lead to an error if applied to states that are changed inside an "ode" block
#'
#' For the help page of the base R \code{fix} function, see \code{\link[utils]{fix}}.
#' @param x a \code{\link{bi_model}} object
#' @param ... values to be assigned to the (named) variables
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- fix(PZ, alpha = 0)
#' @export
fix.bi_model <- function(x, ...) {

  fixed <- list(...)

  ## variables that are to be fixed
  var_str <-
    paste0("^[[:space:]]*(noise|param|state|input|const|obs)[[:space:]]+(",
           paste(names(fixed), collapse = "|"), ")([[:space:][]|$)")
  var_line_nbs <- grep(var_str, x)

  var_vec <- c(var_names(x, "noise"),
               var_names(x, "param"),
               var_names(x, "state"),
               var_names(x, "input"),
               var_names(x, "const"),
               var_names(x, "obs"))

  unmatched_names <- setdiff(names(fixed), var_vec)

  if (length(var_line_nbs) > 0)
  {
    x <- x[-var_line_nbs]
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
      first_const_line <- grep("^[[:space:]]*(noise|param|state|input|const)[[:space:]]+", x)[1]
    }
    x <-
      c(x[1:(first_const_line - 1)],
        fixed_line,
        x[first_const_line:length(x)])
  }

  for (var in intersect(names(fixed), var_vec)) {
    ## remove assignments
    assignments <-
      grep(paste0(var,
                  "(/dt)?[[:space:]]*(\\[[^]]*\\])?[[:space:]]*(~|=|<-)"),
           x)
    if (length(assignments) > 0) {
      x <- x[-assignments]
    }

    ## remove dimensions
    x <-
      gsub(paste0(var, "[[:space:]]*\\[[^]]*\\]"), var, x)

    ## add const assignment
    fixed_line <- paste0("const ", var, " = ", fixed[[var]])
    x <- c(x[1:(var_line_nbs[1] - 1)],
                   fixed_line, 
                   x[(var_line_nbs[1]):length(x)])

  }

  return(clean_model(x))
}
#' @export
fix.default <- function(x, ...) {
  utils::fix(x, ...)
}

#' @name propose_prior
#' @title Propose from the prior in a libbi model
#' @description
#' Generates a version of the model where the proposal blocks are replaced by the prior blocks. This is useful for exploration of the likelihood surface.
#'
#' @param x a \code{\link{bi_model}} object
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @rdname propose_prior
propose_prior <- function(x) {
  new_model <- bi_model(lines = x)

  ## remove parameter proposal
  prior_parameter <- get_block(new_model, "parameter")
  new_model <- add_block(new_model, "proposal_parameter", lines = prior_parameter)

  prior_initial <- get_block(new_model, "initial")
  new_model <- add_block(new_model, "proposal_initial", lines = prior_initial)

  return(clean_model(new_model))
}

#' @name obs_to_noise
#' @title Copy obs variables to state variables (with '__sample_' prepended)
#'
#' @description
#' This is used by the \code{\link{run}} functions of rbi, if 'sample_obs=TRUE'
#'   is specified.
#' @param x a \code{\link{bi_model}} object
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @rdname obs_to_noise
obs_to_noise <- function(x) {
  new_model <- bi_model(lines = x)
  obs_block <- get_block(x, "observation")
  obs_variables <- var_names(x, "obs")

  obs_var_pattern <- paste0("^(", paste(obs_variables, collapse = "|"), ")")
  state_block <- sub(obs_var_pattern, "__sample_\\1", obs_block)
  new_model <- insert_lines(new_model, state_block, at_end_of = "transition")
  state_variables <- paste0("__sample_", obs_variables)
  new_model <- insert_lines(new_model, paste("noise ", paste(state_variables, collapse = ", ")), after = 1)

  return(clean_model(new_model))
}

#' @name clean_model
#' @title Strip model code to its bare bones
#'
#' @description
#' Cleans the model by working out correct indents, removing long comments and
#'   merging lines
#' @param x a \code{\link{bi_model}} object
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
clean_model <- function(x) {
  ## strip comments
  x <- as.character(x)

  x <- sub("//.*$", "", x)

  ## remove long comments and merge lines
  i <- 1
  comment <- FALSE
  while (i <= length(x)) {
    x[i] <- gsub("/\\*[^(\\*)]*\\*/", "", x[i])
    if (grepl("/\\*", x[i])) {
      x[i] <- sub("/\\*.*$", "", x[i])
      comment <- TRUE
    }
    if (grepl("\\*/", x[i])) {
      x[i] <- sub("^.*\\*/", "", x[i])
      comment <- FALSE
    }

    if (comment) {
      x <- x[-i]
    } else {
      if (!comment && grepl("[*-+=/][[:space:]]*$", x[i])) {
        x[i] <- paste(x[i], x[i + 1])
        x <- x[-(i + 1)]
      } else {
        i <- i + 1
      }
    }
  }

  ## remove multiple spaces
  x <- gsub("[[:space:]]+", " ", x)
  ## make sure there is a line break after opening braces 
  x <- gsub("\\{(.+)$", "{\n\\1", x)
  ## make sure there is a line break before closing braces
  x <- gsub("^(.+)\\}", "\\1\n}", x)
  ## replace semicolons with newlines
  x <- gsub(";", "\n", x)
  ## remove trailing spaces
  x <- gsub("[[:space:]]*$", "", x)
  ## remove initial spaces
  x <- gsub("^[[:space:]]*", "", x)
  ## split along newlines
  x <- unlist(strsplit(x, "\n"))
  ## remove empty lines
  x <- x[x!=""]

  ## check
  opening_curls <- length(grep("\\{", x))
  closing_curls <- length(grep("\\}", x))
  if (opening_curls != closing_curls) {
    warning("Model contains unbalanced braces.")
  }
  model <- structure(x, class="bi_model")

  return(model)
}

#' @export
insert_lines <- function(x, ...) UseMethod("insert_lines")
#' @name insert_lines
#' @title Insert lines in a LibBi model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or \code{after} is given, the line(s) will be inserted before or after a given line number or block name, respectively. If one of \code{at_beginning of} or \code{at_end_of} is given, the lines will be inserted at the beginning/end of the block, respectively.
#'
#' @param x a \code{\link{bi_model}} object
#' @param lines vector or line(s)
#' @param before line number before which to insert line(s)
#' @param after line number after which to insert line(s)
#' @param at_beginning_of block at the beginning of which to insert lines(s)
#' @param at_end_of block at the end of which to insert lines(s)
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- insert_lines(PZ, lines = "noise beta", after = 8)
#' @rdname insert_lines
#' @export
insert_lines.bi_model <- function(x, lines, before, after, at_beginning_of, at_end_of, ...) {
  args <- match.call()
  arg_name <- setdiff(names(args), c("", "x", "lines"))
  if (length(arg_name) != 1) {
    stop("insert_lines needs exactly three arguments, 'x', 'lines' and one of 'before', 'after', 'at_beginning_of' or 'at_end_of'")
  }
  arg <- get(arg_name)
  if (is.numeric(arg)) arg <- as.integer(arg)

  if (arg_name %in% c("before", "after") && is.integer(arg)) {
    if (arg_name == "before") {
      after <- before - 1
    }
    if (after > length(x)) {
      stop("model only has ", length(x), " lines, higher requested.")
    }
  } else {
    block_lines <- find_block(x, arg)
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
    x <- c(lines, x)
  } else if (after == length(x)) {
    x <- c(x[1:after], lines)
  } else {
    x <- c(x[1:after], lines, x[(after+1):length(x)])
  }
  return(clean_model(x))
}

#' @export
replace_all <- function(x, ...) UseMethod("replace_all")
#' @name replace_all
#' @title Replace all instances of a string with another in a model
#' @description
#' Takes every occurrence of one string and replaces it with another
#'
#' @param x a \code{\link{bi_model}} object
#' @param from string to be replaced (a regular expression)
#' @param to new string (which can refer to the regular expression given as \code{from})
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @rdname replace_all
#' @export
replace_all.bi_model <- function(x, from, to, ...) {
  x <- gsub(from, to, x)
  return(clean_model(x))
}

#' @export
remove_lines <- function(x, ...) UseMethod("remove_lines")
#' @name remove_lines
#' @title Remove line(s) and/or block(s) in a libbi model
#' @description
#' Removes one or more lines in a libbi model.
#'
#' @param x a \code{\link{bi_model}} object
#' @param what either a vector of line number(s) to remove, or a vector of blocks to remove (e.g., "parameter")
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- remove_lines(PZ, 2)
#' @rdname remove_lines
#' @export
remove_lines.bi_model <- function(x, what, ...) {
  if (missing(what)) {
    stop("'what' must be given")
  }
  if (is.numeric(what)) {
    x <- x[-what]
  } else if (is.character(what)) {
    block <- find_block(x, what)
    if (length(block) > 0) {
      x <- x[-block]
    }
  } else {
    stop("'what' must be a numeric or character vector.")
  }
  return(clean_model(x))
}

#' @export
write_file <- function(x, ...) UseMethod("write_file")
#' @name write_file
#' @title Writes a bi model to a file.
#' @description
#' Writes a bi model to a file given by \code{filename}. The extension '.bi' will be added if necessary.
#'
#' @param x a \code{\link{bi_model}} object
#' @param filename name of the file to be written
#' @param ... ignored
#' @return the return value of the \code{\link{writeLines}} call.
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' write_file(PZ, "PZ.bi")
#' @rdname write_file
#' @export
write_file.bi_model <- function(x, filename, ...) {
  "Write model to file"
  if (!grepl("\\.bi$", filename)) {
    filename <- paste(filename, "bi", sep = ".")
  }
  model_name <- sub("\\.bi$", "", basename(filename))

  writeLines(as.character(x), con = filename, sep = "\n")
}

#' @name find_block
#' @title Find a block in a LibBi model
#'
#' @description
#' Finds a block and returns the range of line numbers encompassed by that block.
#' @return range of line numbers
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
#' @param name of the block to find
#' @rdname find_block
find_block <- function(x, name) {
  lines <- as.character(x)
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

#' @export
get_block <- function(x, ...) UseMethod("get_block")
#' @name get_block
#' @title Get the contents of a block in a LibBi model
#'
#' @description
#' Returns the contents of a block in a LibBi model as a character vector of
#'   lines.
#' @return a character vector of the lines in the block
#' @param x a \code{\link{bi_model}} object
#' @param name name of the block
#' @param ... ignored
#' @rdname get_block
#' @export
get_block.bi_model <- function(x, name, ...) {
  block <- find_block(x, name)
  if (length(block) > 0) {
    lines <- as.character(x[block])
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

#' @export
add_block <- function(x, ...) UseMethod("add_block")
#' @name add_block
#' @title Add a block to a LibBi model
#'
#' @description
#' Add a block to a LibBi model. If that block exists, it will be removed first.
#' @return a \code{\link{bi_model}} object containing the new block
#' @param x a \code{\link{bi_model}} object
#' @param name name of the block
#' @param lines character vector, lines in the block
#' @param options any options to the block
#' @param ... ignored
#' @rdname add_block
#' @export
add_block.bi_model <- function(x, name, lines, options, ...) {
  x <- remove_lines(x, what=name)
  x <- c(x[seq_len(length(x) - 1)],
         ifelse(missing(options), paste("sub", name,"{"),
                paste("sub", name, paste0("(", options, ")", "{"))), 
         paste(lines, sep = "\n"), "}", "}")
  x <- clean_model(x)
}

#' @export
var_names <- function(x, ...) UseMethod("var_names")
#' @name var_names
#' @title Get variables
#' @description
#' Get all variable names of one or more type(s)
#'
#' This returns all variable names of a certain type ("param", "state", "obs", "noise", "const") contained in the model of a \code{\link{libbi}} object
#' @param x a \code{\link{bi_model}} object
#' @param type a character vector of one or more types
#' @param dim logical; if set to TRUE, names will contain dimensions in brackets
#' @param opt logical; if set to TRUE, names will contain options (e.g., has_output)
#' @param ... ignored
#' @return variable names
#' @rdname var_names
#' @export
var_names.bi_model <- function(x, type, dim = FALSE, opt = FALSE, ...) {
  names_vec <- c()
  for (for_type in type) {
    line_nbs <- grep(paste0("^[[:space:]]*", for_type, "[[:space:]]"), x)
    if (length(line_nbs) > 0) {
      ## remove qualifier
      names <- sub(paste0("^[[:space:]]*", for_type, "[[:space:]]"), "", x[line_nbs])
      if (!dim) {
        ## remove dimensions
        names <- sub("\\[.*\\]", "", names)
      }
      if (!opt) {
        ## remove options
        names <- sub("\\(.*\\)", "", names)
      }
      if (for_type == "const") {
        ## remove assignments
        names <- sub("=.*$", "", names)
      }
      ## remove spaces
      names <- gsub("[[:space:]]", "", names)
      names_vec <- c(names_vec, unlist(strsplit(names, ",")))
    }
  }
  return(names_vec)
}

#' @title Print the lines of a LibBi model
#'
#' @description
#' Prints all lines in a LibBi model
#' @param x a \code{\link{bi_model}} object
#' @param spaces number of spaces for indentation
#' @param ... ignored
#' @rdname print.bi_model
#' @name print.bi_model
#' @keywords internal
#' @export
print.bi_model <- function(x, spaces=2, ...) {
  name <- get_name(x)
  if (!is.na(name)) {
    cat("bi model:", name, "\n")
    cat("==========", paste(rep("=", nchar(name)), collapse = ""), "\n",
        sep = "")
  } else {
    cat("unnamed bi model\n")
    cat("================\n")
  }
  if (length(x) == 0) {
    cat("// empty", "\n")
  } else {
    vec <- c()
    indent <- 0
    for (i in seq_along(x)) {
      if (grepl("\\}", x[i])) {
        if (indent > 0) {
          indent <- indent - 1
        } else {
          warning("There seems to be a pair of unbalanced braces")
        }
      }
      indent_spaces <- paste(rep(" ", indent * spaces), collapse = "")
      vec <- c(vec, paste0(indent_spaces, x[i]))
      if (grepl("\\{", x[i])) {
        indent <- indent + 1
      }
    }
    print(vec)
  }
}

#' @name is_empty
#' @title Check if a model is empty
#' @description
#' Checks if a model is empty (i.e., has been initialised without any content)
#'
#' @param x a \code{\link{bi_model}} object
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @rdname is_empty
is_empty <- function(x) {
  return(length(x) == 0)
}

#' @export
get_name <- function(x, ...) UseMethod("get_name")
#' @name get_name
#' @title Get the name of a bi model
#' @description
#' Extracts the name of a bi model (first line of the .bi file).
#'
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return the name of the model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' get_name(PZ)
#' @rdname get_name
#' @export
get_name.bi_model <- function(x, ...) {
  if (length(x) > 0) {
    name <- gsub("model ", "", gsub("\\{", "", x[1]))
    name <- gsub("[[:space:]]*$", "", name)
    name <- gsub("^[[:space:]]*", "", name)
  } else {
    name <- NA_character_
  }
  return(name)
}

#' @export
set_name <- function(x, ...) UseMethod("set_name")
#' @name set_name
#' @title Set the name of a bi model
#' @description
#' Changes the name of a bi model (first line of the .bi file) to the specified name.
#'
#' @param name Name of the model
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- set_name(PZ, "new_PZ")
#' @rdname set_name
#' @export
set_name.bi_model <- function(x, name, ...) {
  if (length(x) > 0) {
    if (grepl("model [[:graph:]]+ \\{", x[1])) {
      x[1] <-
        sub("model [[:graph:]]+ \\{", paste0("model ", name, " {"),
            x[1])
    } else {
      stop("could not identify model name in first line")
    }
  } else {
    x <- c(paste0("model ", name, " {"), "}")
  }
  clean_model(x)
}

#' @name Extract.bi_model
#' @rdname Extract.bi_model
#' @title Subset and replace model lines
#' @aliases `[<-.bi_model`
#' @description
#' Extracts a subset of lines from the model and, if used with the assignment
#'   operator, assigns new character strings.
#' @param x A bi_model
#' @param i A vector of line numbers
#' @param ... ignored
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:4]
#' PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
#' @export
#' @param value A vector of the same length as \code{i}, containing the
#'   replacement strings
`[<-.bi_model` <-  function(x, i, ..., value) {
    model_char <- as.character(x)
    model_char[i] <- value
    return(clean_model(model_char))
}
