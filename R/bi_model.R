#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file, URL
#'   or character vector.  Once the instance is created, the model can be fed to
#'   a \code{\link{libbi}} object.
#'
#' @param filename the file name of the model file
#' @param lines lines of the model (if no \code{filename} is given), a character
#'   vector
#' @param ... ignored
#' @return a \code{{bi_model}} object containing the newly created model
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' @seealso \code{\link{fix}}, \code{\link{insert_lines}},
#'   \code{\link{remove_lines}}, \code{\link{replace_all}},
#'   \code{\link{get_name}}, \code{\link{set_name}}, \code{\link{write_model}}
#' @export
bi_model <- function(filename, lines, ...) {
  if (sum(!missing(filename), !missing(lines)) > 1) {
    stop("Only one of 'filename' or 'lines' can be given")
  }
  if (!missing(filename)) {
    if (length(as.character(filename)) == 0) {
      stop("Filename must be a non-empty character string")
    }
  }

  if (!missing(filename)) {
    model <- readLines(filename)
  } else if (!missing(lines)) {
    model <- lines
  } else {
    model <- character(0)
  }

  return(clean_model(model))
}

#' @rdname remove_vars
#' @name remove_vars
#' @title Remove variables
#' @description
#' Removes variables from the left-hand side of a model
#'
#' Used by \code{\link{fix}} and \code{\link{to_input}}
#' @param x a \code{\link{bi_model}} object
#' @param vars vector of variables to remove
#' @return a bi model object of the new model
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
remove_vars <- function(x, vars) {
  ## remove variable declarations
  var_str <- paste0(
    "^[[:space:]]*(noise|param|state|input|const|obs)[[:space:]]+(",
    paste(vars, collapse = "|"), ")([[:space:][\\=~]|$)"
  )
  var_line_nbs <- grep(var_str, x)

  save_var_names <- var_names(x)

  if (length(var_line_nbs) > 0) {
    x <- x[-var_line_nbs]
  }

  for (var in intersect(vars, save_var_names)) {
    ## remove assignments
    assignments <- grep(
      paste0(
        "^[[:space:]]*", var,
        "(/dt)?[[:space:]]*(\\[[^]]*\\])?[[:space:]]*(~|=|<-)"
      ), x
    )
    ode_assignments <- grep(
      paste0(
        "^[[:space:]]*d", var,
        "/dt[[:space:]]*(\\[[^]]*\\])?[[:space:]]*(~|=|<-)"
      ), x
    )
    all_assignments <- c(assignments, ode_assignments)
    if (length(all_assignments) > 0) {
      x <- x[-all_assignments]
    }
  }

  return(clean_model(x))
}

#' @rdname to_input
#' @name to_input
#' @title Convert variables into inputs
#' @description
#' Used for predictions, if one doesn't want to re-simulate state/noise
#'   trajectories
#'
#' @param x a \code{\link{bi_model}} object
#' @param vars vector of variables to convert to inputs
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @keywords internal
to_input <- function(x, vars) {
  ## only consider state or noise variables
  var_names_args <- list(x, type = c("state", "noise"))
  if (!missing(vars)) {
    var_names_args[["vars"]] <- vars
  }
  sn_vars <- do.call(var_names, var_names_args)
  ## get full variable names, with dimensions
  dim_vars <- var_names(x, vars = sn_vars, dim = TRUE)
  names(dim_vars) <- sn_vars

  ## remove variables
  x <- remove_vars(x, sn_vars)

  input_lines <- c()
  for (var in sn_vars) {
    ## add input definition after dimensions are defined
    input_lines[var] <- paste("input", dim_vars[[var]])
  }
  if (length(input_lines) > 0) {
    dim_lines <- grep("^[[:space:]]*dim[[:space:]]", x)
    if (length(dim_lines) == 0) {
      dim_lines <- 1
    }
    x <- insert_lines(x, input_lines, after = max(dim_lines))
  }

  return(clean_model(x))
}

#' @rdname enable_outputs
#' @name enable_outputs
#' @title Enable outputting variables in a \code{\link{bi_model}}
#' @description
#' Any variable type given will have any 'has_output=0' option removed in the
#'   given model.
#'
#' @param x a \code{\link{bi_model}} object
#' @param type either "all" (default), or a vector of variable types that are to
#'   have outputs enabled
#' @return the updated \code{bi_model} object
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[6] <- "param mu (has_output=0)"
#' PZ <- enable_outputs(PZ)
#' @seealso \code{\link{bi_model}}
#' @export
enable_outputs <- function(x, type = "all") {
  if (!("bi_model") %in% class(x)) {
    stop("'x' must be a 'bi_model' object.")
  }
  if (length(type) > 1 && "all" %in% type) {
    stop("'type=\"all\"' only makes sense on its own")
  }
  if ("all" %in% type) {
    type <- c("param", "state", "noise")
  }
  opt_vars <- var_names(x, type = type, opt = TRUE)
  no_output_pattern <-
    "([[:space:](,])(has_output[[:space:]]*=[[:space:]]*0[[:space:]]*)([,)])"
  no_output <- grep(no_output_pattern, opt_vars)
  noopt_nooutput_vars <- var_names(x, type = type)[no_output]
  var_lines <- grep(
    paste0(
      "^[[:space:]]*(", paste(type, collapse = "|"), ")[[:space:]]*(",
      paste(noopt_nooutput_vars, collapse = "|"), ")[[:space:]([]"
    ), x
  )

  updated_lines <- sub(no_output_pattern, "\\1\\3", x[var_lines])
  updated_lines <- gsub(",,", ",", updated_lines)
  updated_lines <- gsub("\\(,", "(", updated_lines)
  updated_lines <- gsub(",\\)", ")", updated_lines)
  updated_lines <- sub("[[:space:]]*\\(\\)", "", updated_lines)
  x[var_lines] <- updated_lines
  return(clean_model(x))
}

#' @export
fix <- function(x, ...) UseMethod("fix")
#' @rdname fix
#' @name fix
#' @title Fix noise term, state or parameter of a libbi model
#' @description
#' Replaces all variables with fixed values as given ; note that this will not
#'   replace differential equations and lead to an error if applied to states
#'   that are changed inside an "ode" block
#'
#' For the help page of the base R \code{fix} function, see
#'   \code{\link[utils]{fix}}.
#' @param x a \code{\link{bi_model}} object
#' @param ... values to be assigned to the (named) variables
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- fix(PZ, alpha = 0)
#' @export
fix.bi_model <- function(x, ...) {
  fixed <- list(...)

  x <- remove_vars(x, names(fixed))

  for (var in names(fixed)) {
    ## remove dimensions
    x <- gsub(
      paste0("(^|[^a-zA-Z_0-0])", var, "[[:space:]]*\\[[^]]*\\]"),
      paste0("\\1", var), x
    )

    ## add const assignment
    fixed_line <- paste0("const ", var, " = ", fixed[[var]])
    x <- c(x[1], fixed_line, x[2:length(x)])
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
#' Generates a version of the model where the proposal blocks are replaced by
#'   the prior blocks. This is useful for exploration of the likelihood surface.
#'
#' @param x a \code{\link{bi_model}} object
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @rdname propose_prior
propose_prior <- function(x) {
  new_model <- bi_model(lines = x)

  ## remove parameter proposal
  prior_parameter <- get_block(new_model, "parameter")
  new_model <- add_block(
    new_model, "proposal_parameter", lines = prior_parameter
  )

  prior_initial <- get_block(new_model, "initial")
  new_model <- add_block(new_model, "proposal_initial", lines = prior_initial)

  return(clean_model(new_model))
}

#' @name clean_model
#' @title Strip model code to its bare bones
#'
#' @description
#' Cleans the model by working out correct indents, removing long comments and
#'   merging lines
#' @param x a \code{\link{bi_model}} object
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @keywords internal
clean_model <- function(x) {
  x <- as.character(x)

  ## strip comments starting with //
  x <- sub("//.*$", "", x)

  ## remove long comments and merge lines
  i <- 1
  comment <- FALSE # flag to denote whether a line starts inside a comment
  while (i <= length(x)) {
    ## remove sections delimited by /* ... */ within lines
    x[i] <- gsub("/\\*[^(\\*)]*\\*/", "", x[i])
    if (comment) { ## we're inside a comment
      if (grepl("\\*/", x[i])) { ## comment ends but does not start on this line
        ## Remove everything before */ and unset the 'comment' flag so that in
        ## the next line we know that we're not in a comment any longer
        x[i] <- sub("^.*\\*/", "", x[i])
        comment <- FALSE
      } else { ## comment does not end on this line -- we remove the line
        x <- x[-i]
      }
    }

    ## if 'comment' is still true, we're inside the comment -- move on
    if (!comment) { ## we're not inside a comment
      if (grepl("/\\*", x[i])) {
        ## comment starts but does not end on this line.
        ## Remove everything after /* and set the 'comment' flag so that in the
        ## next line we know we're still inside the comment
        x[i] <- sub("/\\*.*$", "", x[i])
        comment <- TRUE
      }
      if (grepl("[*-+=/%][[:space:]]*$", x[i])) {
        ## line ends on an operator -- merge lines
        x[i] <- paste(x[i], x[i + 1])
        x <- x[-(i + 1)]
      } else {
        i <- i + 1
      }
    }
  }

  ## split multiple declarations in a single line
  comma_lines <-
    paste0(
      "^[[:space:]]*(noise|param|state|input|const|obs)[[:space:]]+",
      "([^[:space:]].*,.*$)"
    )
  comma_line_nbs <- grep(comma_lines, x)

  for (id in seq_along(comma_line_nbs)) {
    line_nb <- comma_line_nbs[id]
    type <- sub(comma_lines, "\\1", x[line_nb])
    type_vars <- sub(comma_lines, "\\2", x[line_nb])
    vars <- unlist(strsplit(
      type_vars, "[([][^)\\]]+,(*SKIP)(*FAIL)|,\\s*", perl = TRUE
    ))
    vars <- gsub("[[:space:]]", "", vars)
    new_lines <- paste(type, vars)
    x <- c(x[1:(line_nb - 1)], new_lines, x[(line_nb + 1):length(x)])
    comma_line_nbs <- comma_line_nbs + length(new_lines) - 1
  }

  ## remove multiple spaces
  x <- gsub("[[:space:]]+", " ", x)
  ## make sure there is a line break after opening braces
  x <- gsub("\\{", "{\n", x)
  ## make sure there is a line break before closing braces
  x <- gsub("\\}", "\n}", x)
  ## replace semicolons with newlines
  x <- gsub(";", "\n", x)
  ## split along newlines
  x <- unlist(strsplit(x, "\n"))
  ## remove trailing spaces
  x <- gsub("[[:space:]]*$", "", x)
  ## remove initial spaces
  x <- gsub("^[[:space:]]*", "", x)
  ## remove empty lines
  x <- x[x != ""]

  ## check
  opening_curls <- length(grep("\\{", x))
  closing_curls <- length(grep("\\}", x))
  if (opening_curls != closing_curls) {
    warning("Model contains unbalanced braces.")
  }
  model <- structure(x, class = "bi_model")

  return(model)
}

#' @export
insert_lines <- function(x, ...) UseMethod("insert_lines")
#' @name insert_lines
#' @title Insert lines in a LibBi model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or
#'   \code{after} is given, the line(s) will be inserted before or after a given
#'   line number or block name, respectively. If one of \code{at_beginning of}
#'   or \code{at_end_of} is given, the lines will be inserted at the
#'   beginning/end of the block, respectively.
#'
#' @param x a \code{\link{bi_model}} object
#' @param lines vector or line(s)
#' @param before line number before which to insert line(s)
#' @param after line number after which to insert line(s)
#' @param at_beginning_of block at the beginning of which to insert lines(s)
#' @param at_end_of block at the end of which to insert lines(s)
#' @param ... ignored
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- insert_lines(PZ, lines = "noise beta", after = 8)
#' @rdname insert_lines
#' @export
insert_lines.bi_model <- function(x, lines, before, after, at_beginning_of,
                                  at_end_of, ...) {
  args <- match.call()
  arg_name <- setdiff(names(args), c("", "x", "lines"))
  if (length(arg_name) != 1) {
    stop(
      "insert_lines needs exactly three arguments, 'x', 'lines' and one of ",
      "'before', 'after', 'at_beginning_of' or 'at_end_of'"
    )
  }
  arg <- eval.parent(args[[arg_name]])
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
    if (length(block_lines) == 0) {
      x <- add_block(x, arg)
      block_lines <- find_block(x, arg)
    }
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
  }

  if (after == 0) {
    x <- c(lines, x)
  } else if (after == length(x)) {
    x <- c(x[1:after], lines)
  } else {
    x <- c(x[1:after], lines, x[(after + 1):length(x)])
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
#' @param to new string (which can refer to the regular expression given as
#'   \code{from})
#' @param ... ignored
#' @return the updated \code{bi_model} object
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
#' @param what either a vector of line number(s) to remove, or a vector of
#'   blocks to remove (e.g., "parameter")
#' @param only only remove lines assigning given names (as a vector of character
#'   strings)
#' @param type which types of lines to remove, either "all", "sample" (i.e.,
#'   lines with a "~") or "assignment" (lines with a "<-" or "=") (default:
#'   "all")
#' @param preserve_shell if TRUE (default: FALSE), preserve the definition of a
#'   block even if all lines are removed; this is useful to preserve options
#'   passed to a \code{transition} or \code{ode} block
#' @param ... ignored
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- remove_lines(PZ, 2)
#' @rdname remove_lines
#' @export
remove_lines.bi_model <- function(x, what, only,
                                  type = c("all", "assignment", "sample"),
                                  preserve_shell = FALSE, ...) {
  if (missing(what)) {
    stop("'what' must be given")
  }
  type <- match.arg(type)
  to_remove <- c()
  if (is.numeric(what)) {
    to_remove <- what
  } else if (is.character(what)) {
    to_remove <- find_block(x, what, inner = preserve_shell)
  } else {
    stop("'what' must be a numeric or character vector.")
  }

  operators <- list(assignment = c("=", "<-"), sample = "~")

  ## check if we don't want to remove everything
  if (length(to_remove) > 0 && (type != "all" || !missing(only))) {
    if (type == "all") {
      op_types <- unlist(operators)
    } else {
      op_types <- operators[[type]]
    }
    pattern <- paste0(
      "^(const)?[[:space:]]*([A-Za-z_0-9[\\]][[:space:]A-Za-z_0-9,[\\]]*)",
      "(", paste(op_types, collapse = "|"), ")"
    )
    assign_lines <- grep(pattern, x[to_remove], perl = TRUE)
    assign_vars <- sub(paste0(pattern, ".*$"), "\\2",
      x[to_remove][assign_lines],
      perl = TRUE
    )
    assign_vars <- sub("[[:space:]]", "", sub("\\[.*]", "", assign_vars))
    if (!missing(only)) {
      assign_lines <- assign_lines[assign_vars %in% only]
    }
    if (is.character(what) && !preserve_shell &&
      length(assign_lines) == length(to_remove) - 2) {
      assign_lines <- c(1, assign_lines, length(to_remove))
    }
    to_remove <- to_remove[assign_lines]
  }

  if (length(to_remove) > 0) {
    x <- x[-to_remove]
  }

  return(clean_model(x))
}


#' @export
write_model <- function(x, ...) UseMethod("write_model")
#' @name write_model
#' @title Writes a bi model to a file.
#' @description
#' Writes a bi model to a file given by \code{filename}. The extension '.bi'
#'   will be added if necessary.
#'
#' @param x a \code{\link{bi_model}} object, or a \code{\link{libbi}} object
#'   containing a model
#' @param filename name of the file to be written
#' @param update.name whether to update the model name with the file name
#' @param ... ignored
#' @return the return value of the \code{\link{writeLines}} call.
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' new_file_name <- tempfile("PZ", fileext = ".bi")
#' write_model(PZ, new_file_name)
#' @rdname write_model
#' @export
write_model.bi_model <- function(x, filename, update.name = TRUE, ...) {
  "Write model to file"
  if (!grepl("\\.bi$", filename)) {
    filename <- paste(filename, "bi", sep = ".")
  }
  model_name <- sub("\\.bi$", "", basename(filename))
  if (update.name) {
    x <- set_name(x, model_name)
  }

  writeLines(print(x, screen = FALSE), con = filename, sep = "\n")
}
#' @rdname write_model
#' @name write_model
#' @export
write_model.libbi <- function(x, filename, ...) {
  if (missing(filename)) {
    filename <- x$model_file_name
  }
  write_model(x$model, filename = filename, ...)
}

#' @export
find_block <- function(x, ...) UseMethod("find_block")
#' @name find_block
#' @title Find a block in a LibBi model
#'
#' @description
#' Finds a block and returns the range of line numbers encompassed by that
#'   block.
#' @return an integerr vector, the range of line numbers
#' @seealso \code{\link{bi_model}}
#' @keywords internal
#' @param x a \code{\link{bi_model}} object
#' @param name of the block to find
#' @param inner only return the inner part of the block (not the block
#'   definition)
#' @rdname find_block
find_block.bi_model <- function(x, name, inner = FALSE) {
  lines <- as.character(x)
  sub_regexp <- paste0(
    "^[[:space:]]*(sub[[:space:]]+)?[[:space:]]*", name,
    "([[:space:]][a-zA-Z0-9_\\.]+)?[[:space:]]*(\\(.*\\))?[[:space:]]*\\{"
  )
  sub_line <- grep(sub_regexp, lines)
  if (length(sub_line) == 1) {
    lines[sub_line] <- sub(sub_regexp, "", lines[sub_line])
    open_braces <- 1
    line <- sub_line - 1
    while (open_braces > 0) {
      line <- line + 1
      open_braces <- open_braces +
        nchar(gsub("\\}", "", lines[line])) -
        nchar(gsub("\\{", "", lines[line]))
    }
    if (inner) {
      return((sub_line + 1):(line - 1))
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
#' @param shell if TRUE (default:FALSE), will return the shell (i.e., the
#'   definition of the block) as well as content; this is useful, e.g., to see
#'   options passed to a \code{transition} or \code{ode} block
#' @param ... ignored
#' @rdname get_block
#' @export
get_block.bi_model <- function(x, name, shell = FALSE, ...) {
  if (missing(name)) {
    stop("The name of the block must be provided as 'name'")
  }
  block <- find_block(x, name)
  if (length(block) > 0) {
    lines <- as.character(x[block])
    if (!shell) {
      lines <- lines[-c(1, length(lines))]
    }
    return(lines)
  } else {
    return(character(0))
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
  x <- remove_lines(x, what = name)
  if (missing(lines)) {
    lines <- c()
  }
  x <- c(
    x[seq_len(length(x) - 1)],
    ifelse(
      missing(options),
      paste("sub", name, "{"),
      paste("sub", name, paste0("(", options, ")", "{"))
    ),
    paste(lines, sep = "\n"), "}", "}"
  )
  clean_model(x)
}

#' @name var_names
#' @title Get variable names in a LibBi model
#' @description
#' Get variable names of one or more type(s)
#'
#' This returns all variable names of a certain type ("param", "state", "obs",
#'   "noise", "const") contained in the model of a \code{\link{libbi}} object
#' @param x a \code{\link{bi_model}} object
#' @param vars a character vector of variable names; if given, only these
#'   variables names will be considered
#' @param type a character vector of one or more types
#' @param dim logical; if set to TRUE, names will contain dimensions in brackets
#' @param opt logical; if set to TRUE, names will contain options (e.g.,
#'   has_output)
#' @param aux logical; if set to TRUE, auxiliary names will be returned
#' @return a character vector ofvariable names
#' @rdname var_names
#' @export
var_names <- function(x, vars, type, dim = FALSE, opt = FALSE, aux = FALSE) {
  names_vec <- c()
  if (missing(type)) {
    type <- c("param", "state", "input", "const", "obs", "noise")
  }

  for (for_type in type) {
    line_nbs <- grep(paste0("^[[:space:]]*", for_type, "[[:space:]]"), x)
    if (length(line_nbs) > 0) {
      ## remove qualifier
      name <- sub(
        paste0("^[[:space:]]*", for_type, "[[:space:]]"), "", x[line_nbs]
      )
      clean_name <- name
      if (!dim) {
        ## remove dimensions
        name <- gsub("\\[[^]]*\\]", "", name)
      }
      clean_name <- gsub("\\[[^]]*\\]", "", clean_name)
      if (!opt) {
        ## remove options
        name <- sub("\\([^)]*\\)", "", name)
      }
      clean_name <- sub("\\([^)]*\\)", "", clean_name)
      if (for_type == "const") {
        ## remove assignments
        name <- sub("=.*$", "", name)
      }
      ## remove spaces
      name <- gsub("[[:space:]]", "", name)
      clean_name <- gsub("[[:space:]]", "", clean_name)

      ## add to vector
      if (missing(vars)) {
        names_vec <- c(names_vec, name)
      } else {
        names_vec <- c(names_vec, name[clean_name %in% vars])
      }
    }
  }
  if (!aux) {
    names_vec <- grep("^__", names_vec, invert = TRUE, value = TRUE)
  }
  return(names_vec)
}

#' @name get_dims
#' @title Get dimensions in a LibBi model
#' @description
#' Get dimensions contained in a LibBi model and their sizes
#'
#' @param model a \code{\link{bi_model}} object
#' @param type a character vector of one or more types
#' @return a list of dimensions (as names) and their sizes
#' @export
get_dims <- function(model, type) {
  dim_lines <- grep(
    paste0(
      "^[[:space:]]*dim[[:space:]]+[A-z0-9_]+[[:space:]]*",
      "\\([A-z0-9_]+\\)[[:space:]]*$"
    ), model, value = TRUE)
  const <- get_const(model)

  retval <- list()

  for (dim_line in dim_lines) {
    line <- sub(
      paste0(
        "^[[:space:]]*dim[[:space:]]+([A-z0-9_]+)[[:space:]]*",
        "\\(([A-z0-9_]+)\\)[[:space:]]*$"
      ), "\\1|\\2", dim_line
    )
    dim_def <- strsplit(line, "\\|")[[1]]
    if (dim_def[2] %in% names(const)) {
      retval[[dim_def[1]]] <- const[[dim_def[2]]]
    } else {
      try_numeric <- suppressWarnings(as.numeric(dim_def[2]))
      if (is.na(try_numeric)) {
        stop(
          "Can't determine size of dimension '", dim_def[1], ": ", dim_def[2]
        )
      } else {
        retval[[dim_def[1]]] <- try_numeric
      }
    }
  }
  return(retval)
}

#' @name get_const
#' @title Get constants in a LibBi model
#' @description
#' Get constants contained in a LibBi model and their values. This will attempt
#'   to evaluate any calculation on the right hand side. Failing that, it will
#'   be returned verbatim.
#'
#' @param model a \code{\link{bi_model}} object
#' @return a list of constants (as names) and their values
#' @export
get_const <- function(model) {
  const_lines <- grep(
    "^[[:space:]]*const[[:space:]].*=.*[^[:space:]]", model, value = TRUE
  )
  retval <- list()
  for (const_line in const_lines) {
    line <- gsub(
      " ", "", sub("^[[:space:]]*const[[:space:]]*", "", const_line)
    )
    assignment <- strsplit(line, "=")[[1]]
    retval[assignment[1]] <- NA_character_
    retval[[assignment[[1]]]] <- tryCatch({
      eval(parse(text = assignment[2]), envir = NULL, enclos = NULL)
    }, error = function(cond) {
      assignment[2]
    })
  }
  return(retval)
}

#' @title Print the lines of a LibBi model
#'
#' @description
#' Prints all lines in a LibBi model
#' @param x a \code{\link{bi_model}} object
#' @param spaces number of spaces for indentation
#' @param screen whether to print to screen (default: TRUE). In that case, line
#'   numbers will be added; otherwise, a character vector will be returned.
#' @param ... ignored
#' @rdname print
#' @name print.bi_model
#' @return if \code{screen} is \code{FALSE}, a character vector of model lines
#' @keywords internal
#' @export
print.bi_model <- function(x, spaces = 2, screen = TRUE, ...) {
  x <- clean_model(x)
  if (screen) {
    cat("bi_model:\n")
    cat("=========\n")
  }
  if (length(x) == 0) {
    if (screen) cat("// empty", "\n")
  } else {
    vec <- c()
    indent <- 0
    for (i in seq_along(x)) {
      if (grepl("\\}", x[i])) indent <- indent - 1
      indent_spaces <- paste(rep(" ", max(0, indent * spaces)), collapse = "")
      vec <- c(vec, paste0(indent_spaces, x[i]))
      if (grepl("\\{", x[i])) {
        indent <- indent + 1
      }
    }
    if (screen) {
      line_num_indent <- nchar(as.character(length(vec)))
      line_nums <- vapply(seq_along(vec), function(y) {
        paste0(
          rep(" ", line_num_indent - nchar(as.character(y))), y, collapse = ""
        )
      }, " ")
      cat(paste(paste(line_nums, vec, sep = ": "), collapse = "\n"), sep = "\n")
    } else {
      return(vec)
    }
  }
}

#' @name is_empty
#' @title Check if a model is empty
#' @description
#' Checks if a model is empty (i.e., has been initialised without any content)
#'
#' @param x a \code{\link{bi_model}} object
#' @return TRUE or FALSE, depending on whether the model is empty
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
#' @return a character string, the name of the model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' get_name(PZ)
#' @rdname get_name
#' @export
get_name.bi_model <- function(x, ...) {
  if (length(x) > 0) {
    name <- gsub("^model[[:space:]]*", "", gsub("\\{", "", x[1]))
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
#' Changes the name of a bi model (first line of the .bi file) to the specified
#'   name.
#'
#' @param name Name of the model
#' @param x a \code{\link{bi_model}} object
#' @param ... ignored
#' @return the updated \code{bi_model} object
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ <- set_name(PZ, "new_PZ")
#' @rdname set_name
#' @export
set_name.bi_model <- function(x, name, ...) {
  if (length(x) > 0) {
    if (grepl("model [[:graph:]]+[[:space:]]*\\{", x[1])) {
      x[1] <- sub(
        "model [[:graph:]]+[[:space:]]*\\{", paste0("model ", name, " {"), x[1]
      )
    } else {
      stop("could not identify model name in first line")
    }
  } else {
    x <- c(paste0("model ", name, " {"), "}")
  }
  clean_model(x)
}

#' @name Extract_assign.bi_model
#' @rdname Extract_assign.bi_model
#' @title Subset and replace model lines
#' @aliases `[<-.bi_model`
#' @description
#' Extracts a subset of lines from the model and assigns new character strings.
#' @param x A bi_model
#' @param i A vector of line numbers
#' @param ... ignored
#' @return the updated \code{bi_model} object
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
#' @export
#' @param value A vector of the same length as \code{i}, containing the
#'   replacement strings
`[<-.bi_model` <- function(x, i, ..., value) {
  model_char <- as.character(x)
  if (is.null(value)) {
    model_char <- model_char[-i]
  } else {
    model_char[i] <- value
  }
  return(clean_model(model_char))
}

#' @name Extract.bi_model
#' @rdname Extract.bi_model
#' @title Subset model lines
#' @aliases `[.bi_model`
#' @description
#' Extracts a subset of lines from the model.
#' @param x A bi_model
#' @param i A vector of line numbers
#' @param ... ignored
#' @return a character string of the extracted model lines(s)
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ[3:4]
#' @export
`[.bi_model` <- function(x, i, ...) {
  model_char <- as.character(x)
  if (missing(i)) {
    return(model_char)
  } else if (any(i >= 0)) {
    return(model_char[i])
  } else {
    return(clean_model(model_char[i]))
  }
}

#' @name Equals.bi_model
#' @rdname Equals.bi_model
#' @title Check if two models are equal
#' @aliases `==.bi_model`
#' @description
#' Ignores differences in the model name.
#' @param e1 a \code{\link{bi_model}}
#' @param e2 a \code{\link{bi_model}}
#' @param ... ignored
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ == PZ # TRUE
#' @export
#' @return TRUE or FALSE, depending on whether the models are equal or not
`==.bi_model` <- function(e1, e2, ...) { ## nolint
  return(
    length(e1) == length(e2) &&
    all(get_block(e1, "model") == get_block(e2, "model"))
  )
}

#' @name Unequals.bi_model
#' @rdname Unequals.bi_model
#' @title Check if two models are unequal
#' @aliases `!=.bi_model`
#' @description
#' Ignores differences in the model name.
#' @param e1 a \code{\link{bi_model}}
#' @param e2 a \code{\link{bi_model}}
#' @param ... ignored
#' @examples
#' model_file_name <- system.file(package = "rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ != PZ # FALSE
#' @export
#' @return TRUE or FALSE, depending on whether the models are equal or not
`!=.bi_model` <- function(e1, e2, ...) { ## nolint
  return(!(e1 == e2))
}
