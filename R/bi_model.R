#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file.
#' Once the instance is created, the model can be fed to a \code{\link{libbi}} object.
#'
#' @param filename is the file name of the model file
#' @param lines lines of the model (if no \code{filename} is given), a character vector
#' @param ... ignored
#' @importFrom R.oo setConstructorS3
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' @seealso \code{\link{fix_vars}}, \code{\link{insert_lines}}, \code{\link{replace_lines}}, \code{\link{remove_lines}}, \code{\link{write_file}}
#' @export
setConstructorS3("bi_model", enforceRCC=FALSE, function(filename, lines, ...) {
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
})

#' @export fix_vars
#' @name fix_vars
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
#' PZ <- fix_vars(PZ, alpha = 0)
NULL
#' @importFrom R.methodsS3 setMethodS3
#' @rdname fix_vars
#' @name fix_vars.bi_model
#' @S3method fix_vars bi_model
setMethodS3("fix_vars", "bi_model", dontWarn="utils", function(x, ...) {

  fix_model <- x$model
  fixed <- list(...)

  ## variables that are to be fixed
  var_str <-
    paste0("^[[:space:]]*(noise|param|state|input|const|obs)[[:space:]]+(",
           paste(names(fixed), collapse = "|"), ")([[:space:][]|$)")
  var_line_nbs <- grep(var_str, fix_model)

  var_vec <- c(var_names(x, "noise"),
               var_names(x, "param"),
               var_names(x, "state"),
               var_names(x, "input"),
               var_names(x, "const"),
               var_names(x, "obs"))

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
})

#' @name propose_prior
#' @title Propose from the prior in a libbi model
#' @description
#' Generates a version of the model where the proposal blocks are replaced by the prior blocks. This is useful for exploration of the likelihood surface.
#'
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @keywords internal
NULL
#' @importFrom R.methodsS3 setMethodS3
#' @rdname propose_prior
#' @name propose_prior.bi_model
setMethodS3("propose_prior", "bi_model", export=TRUE, function(x, ...) {
  new_model <- bi_model(lines = x$model)

  ## remove parameter proposal
  prior_parameter <- get_block(new_model, "parameter")
  new_model <- add_block(new_model, "proposal_parameter", lines = prior_parameter)

  prior_initial <- get_block(new_model, "initial")
  new_model <- add_block(new_model, "proposal_initial", lines = prior_initial)

  return(clean_model(new_model))
})

#' @name obs_to_noise
#' @title Copy obs variables to state variables (with '__sample_' prepended)
#'
#' @description
#' This is used by the \code{\link{run}} functions of rbi, if 'sample_obs=TRUE'
#'   is specified.
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
NULL
#' @rdname obs_to_noise
#' @name obs_to_noise.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("obs_to_noise", "bi_model", export=FALSE, function(x, ...) {
  new_model <- bi_model(lines = x$model)
  obs_block <- get_block(x, "observation")
  obs_variables <- var_names(x, "obs")

  obs_var_pattern <- paste0("^(", paste(obs_variables, collapse = "|"), ")")
  state_block <- sub(obs_var_pattern, "__sample_\\1", obs_block)
  new_model <- insert_lines(new_model, state_block, at_end_of = "transition")
  state_variables <- paste0("__sample_", obs_variables)
  new_model <- insert_lines(new_model, paste("noise ", paste(state_variables, collapse = ", ")), after = 1)

  return(clean_model(new_model))
})

#' @name clean_model
#' @title Strip model code to its bare bones
#'
#' @description
#' Cleans the model by working out correct indents, removing long comments and
#'   merging lines
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return a \code{\link{bi_model}}
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
NULL
#' @rdname clean_model
#' @name clean_model.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("clean_model", "bi_model", export=FALSE, function(x, ...) {
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
  ## split along newlines
  x$model <- unlist(strsplit(x$model, "\n"))
  ## remove empty lines
  x$model <- x$model[x$model!=""]

  if (length(x$model) > 0) {
    x$name <- gsub("model ", "", gsub("\\{", "", x$model[1]))
    x$name <- gsub("[[:space:]]*$", "", x$name)
    x$name <- gsub("^[[:space:]]*", "", x$name)
  }
  return(x)
})

#' @name get_lines
#' @title Get lines in a libbi model
#' @description
#' Gets a libbi model as character vector.
#'
#' @param x a \code{\link{bi_model}} object
#' @param spaces Number of spaces of indentation
#' @param ... ignored
#' @return the libbi model as character vector
#' @seealso \code{\link{bi_model}}, \code{\link{`[.bi_model`}}
#' @keywords internal
NULL
#' @rdname get_lines
#' @name get_lines.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("get_lines", "bi_model", export=FALSE, function(x, spaces = 2, ...) {
  vec <- c()
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
    vec <- c(vec, paste0(indent_spaces, x$model[i]))
    if (grepl("\\{", x$model[i])) {
      indent <- indent + 1
    }
  }
  return(vec)
})

#' @name insert_lines
#' @export insert_lines
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
NULL
#' @rdname insert_lines
#' @name insert_lines.bi_model
#' @S3method insert_lines bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("insert_lines", "bi_model", function(x, lines, before, after, at_beginning_of, at_end_of, ...) {
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
    if (after > length(x$model)) {
      stop("model only has ", length(x$model), " lines, higher requested.")
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
    x$model <- c(lines, x$model)
  } else if (after == length(x$model)) {
    x$model <- c(x$model[1:after], lines)
  } else {
    x$model <- c(x$model[1:after], lines, x$model[(after+1):length(x$model)])
  }
  return(clean_model(x))
})

#' @name replace_lines
#' @title Update line(s) in a libbi model
#' @description
#' Updates one or more lines in a libbi model.
#'
#' @param x a \code{\link{bi_model}} object
#' @param num line number(s) to update
#' @param lines vector of line(s)
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @keywords internal
NULL
#' @rdname replace_lines
#' @name replace_lines.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("replace_lines", "bi_model", export=FALSE, function(x, num, lines, ...) {
  x$model[num] <- lines
  return(clean_model(x))
})

#' @export remove_lines
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
NULL
#' @rdname remove_lines
#' @name remove.bi_model_lines
#' @S3method remove_lines bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("remove_lines", "bi_model", dontWarn="base", function(x, what, ...) {
  if (missing(what)) {
    stop("'what' must be given")
  }
  if (is.numeric(what)) {
    if (length(grep("[\\{\\}]", x$model[what])) %% 2 == 1) {
      stop("Removing lines would create unbalanced braces.")
    }
    x$model <- x$model[-what]
  } else if (is.character(what)) {
    block <- find_block(x, what)
    if (length(block) > 0) {
      x$model <- x$model[-block]
    }
  } else {
    stop("'what' must be a numeric or character vector.")
  }
  return(clean_model(x))
})

#' @export write_file
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
NULL
#' @importFrom R.methodsS3 setMethodS3
#' @rdname write_file
#' @name write_file.bi_model
#' @S3method write_file bi_model
setMethodS3("write_file", "bi_model", function(x, filename, ...) {
  "Write model to file"
  if (!grepl("\\.bi$", filename)) {
    filename <- paste(filename, "bi", sep = ".")
  }
  model_name <- sub("\\.bi$", "", basename(filename))

  writeLines(get_lines(x), con = filename, sep = "\n")
})

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
#' @param ... ignored
NULL
#' @rdname find_block
#' @name find_block.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("find_block", "bi_model", export=FALSE, function(x, name, ...) {
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
})

#' @export get_block
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
NULL
#' @rdname get_block
#' @name get_block.bi_model
#' @importFrom R.methodsS3 setMethodS3
#' @S3method get_block bi_model
setMethodS3("get_block", "bi_model", function(x, name, ...) {
  block <- find_block(x, name)
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
})

#' @export add_block
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
NULL
#' @rdname add_block
#' @name add_block.bi_model
#' @importFrom R.methodsS3 setMethodS3
#' @S3method add_block bi_model
setMethodS3("add_block", "bi_model", function(x, name, lines, options, ...) {
  x <- remove_lines(x, what=name)
  x$model <- c(x$model[seq_len(length(x$model) - 1)],
               ifelse(missing(options), paste("sub", name,"{"),
                      paste("sub", name, paste0("(", options, ")", "{"))), 
               paste(lines, sep = "\n"), "}", "}")
  x <- clean_model(x)
})

#' @export var_names
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
NULL
#' @rdname var_names
#' @name var_names.bi_model
#' @importFrom R.methodsS3 setMethodS3
#' @S3method var_names bi_model
setMethodS3("var_names", "bi_model", function(x, type, dim = FALSE, opt = FALSE, ...) {
  names_vec <- c()
  for (for_type in type) {
    line_nbs <- grep(paste0("^[[:space:]]*", for_type, "[[:space:]]"), x$model)
    if (length(line_nbs) > 0) {
      ## remove qualifier
      names <- sub(paste0("^[[:space:]]*", for_type, "[[:space:]]"), "",
                   x$model[line_nbs])
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
})

#' @title Print the lines of a LibBi model
#'
#' @description
#' Prints all lines in a LibBi model
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @rdname print
#' @name print.bi_model
#' @importFrom R.methodsS3 setMethodS3
#' @keywords internal
#' @S3method print bi_model
setMethodS3("print", "bi_model", function(x, ...) {
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
})

#' @name is_empty
#' @title Check if a model is empty
#' @description
#' Checks if a model is empty (i.e., has been initialised without any content)
#'
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @keywords internal
NULL
#' @rdname is_empty
#' @name is_empty.bi_model
#' @importFrom R.methodsS3 setMethodS3
setMethodS3("is_empty", "bi_model", export=FALSE, function(x, name, ...) {
  is.null(x$model)
})

#' @export set_name
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
NULL
#' @rdname set_name
#' @name set_name.bi_model
#' @importFrom R.methodsS3 setMethodS3
#' @S3method set_name bi_model
setMethodS3("set_name", "bi_model", function(x, name, ...) {
  if (length(x$model) > 0) {
    if (grepl("model [[:graph:]]+ \\{", x$model[1])) {
      x$model[1] <-
        sub("model [[:graph:]]+ \\{", paste0("model ", name, " {"),
            x$model[1])
    } else {
      stop("could not identify model name in first line")
    }
  } else {
    x$model <- c(paste0("model ", name, " {"), "}")
  }
  clean_model(x)
})

#' @name Extract.bi_model
#' @rdname Extract.bi_model
#' @aliases `[.bi_model` `[<-.bi_model`
#' @title Subset and replace model lines
#' @description
#' Extracts a subset of lines from the model and, if used with the assignment
#'   operator, assigns new character strings.
#' @param x A bi_model
#' @param i A vector of line numbers
#' @param value A vector of the same length as \code{i}, containing the
#'   replacement strings
#' @param ... ignored
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:4]
#' PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
NULL
#' @importFrom R.methodsS3 setMethodS3
#' @export
`[.bi_model` <- function(x, i, ...) {
    return(get_lines(x)[i])
}
#' @importFrom R.methodsS3 setMethodS3
#' @export
`[<-.bi_model` <-  function(x, i, ..., value) {
    x <- replace_lines(x, i, value)
    return(clean_model(x))
}
