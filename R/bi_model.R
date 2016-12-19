#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file.
#' Once the instance is created, the model can either be fed to a \code{\link{libbi}}
#' object.
#'
#' @param filename is the file name of the model file
#' @importFrom methods new
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' @seealso \code{\link{bi_model_fix}}, \code{\link{bi_model_propose_prior}}, \code{\link{bi_model_get_lines}}, \code{\link{bi_model_insert_lines}}, \code{\link{bi_model_update_lines}}, \code{\link{bi_model_remove_lines}}, \code{\link{bi_model_set_name}}, \code{\link{bi_model_write}}, \code{\link{bi_model_clone}}
#' @export bi_model
NULL
#' @rdname bi_model_fix
#' @name bi_model_fix
#' @title Fix noise term, state or parameter of a libbi model
#' @description
#' Replaces all variables with fixed values as given in 'fixed'; note that this will not replace differential equations and lead to an error if applied to states that are changed inside an "ode" block
#'
#' @param ... values to be assigned to the (named) variables
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$fix(alpha = 0)
NULL
#' @rdname bi_model_propose_prior
#' @name bi_model_propose_prior
#' @title Propose from the prior in a libbi model
#' @description
#' Generates a version of the model where the proposal blocks are replaced by the prior blocks. This is useful for exploration of the likelihood surface.
#'
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$propose_prior()
NULL
#' @rdname bi_model_get_lines
#' @name bi_model_get_lines
#' @title Get lines in a libbi model
#' @description
#' Gets a libbi model as character vector.
#'
#' @param spaces Number of spaces of indentation
#' @return the libbi model as character vector
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$get_lines()
NULL
#' @rdname bi_model_insert_lines
#' @name bi_model_insert_lines
#' @title Insert lines in a libbi model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or \code{after} is given, the line(s) will be inserted before or after a given line number or block name, respectively. If one of \code{at_beginning of} or \code{at_end_of} is given, the lines will be inserted at the beginning/end of the block, respectively
#'
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
#' PZ$insert_lines(lines = "noise beta", after = 8)
NULL
#' @rdname bi_model_update_lines
#' @name bi_model_update_lines
#' @title Update line(s) in a libbi model
#' @description
#' Updates one or more lines in a libbi model.
#'
#' @param num line number(s) to update
#' @param lines vector of line(s)
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$update_lines(23, "alpha ~ normal(mu, sigma)")
NULL
#' @rdname bi_model_remove_lines
#' @name bi_model_remove_lines
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
#' PZ$remove_lines(2)
NULL
#' @rdname bi_model_replace_all
#' @name bi_model_replace_all
#' @title replace all instances of one string with another
#' @description
#' Replace all instances of one string with another in a libbi model
#'
#' @param old the string to be replaced
#' @param new the string to replace the old string
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$replace_all("alpha", "beta")
NULL
#' @rdname bi_model_set_name
#' @name bi_model_set_name
#' @title Set the name of a bi model
#' @description
#' Changes the name of a bi model (first line of the .bi file) to the specified name.
#'
#' @param name Name of the model
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$set_name("new_PZ")
NULL
#' @rdname bi_model_write
#' @name bi_model_write
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
NULL
#' @rdname bi_model_clone
#' @name bi_model_clone
#' @title Clones a model (returning a new object with the same properties)
#' @description
#' Returns a copy of the model with exactly the same content as the original model.
#' @return cloned model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="rbi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ2 <- PZ$clone()
NULL

bi_model <- setRefClass("bi_model",
      fields = list(model = "character",
                    name = "character"),
      methods = list(
        initialize = function(filename, lines) {
          if (!missing(filename) && !missing(lines)) {
            stop("Only one of 'filename' or 'lines' can be given")
          }
          if (!missing(filename)) {
            if (length(as.character(filename)) == 0) {
              stop ("Filename must be a non-empty character string")
            }
          }

          if (!missing(filename)) {
            model <<- readLines(filename)
          } else if (!missing(lines)) {
            model <<- lines
          } else {
            model <<- character(0)
          }

          clean_model()
        },
        fix = function(...){
          "Fix a parameter, variable or noise term in the model"

            fix_model <- model

            fixed = list(...)

            ## variables that are to be fixed
            var_str <-
                paste0("^[[:space:]]*(noise|param|state|input|const)[[:space:]]+(",
                       paste(names(fixed), collapse = "|"), ")([[:space:][]|$)")
            var_line_nbs <- grep(var_str, fix_model)

            var_vec <- c(.self$get_vars("noise"),
                         .self$get_vars("param"),
                         .self$get_vars("state"),
                         .self$get_vars("input"),
                         .self$get_vars("const"))

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

            model <<- fix_model
            clean_model()
        },
        propose_prior = function() {
          "Change a libbi model to make the prior the proposal distribution"
          new_model <- bi_model(lines = .self$model)

          ## remove parameter proposal
          prior_parameter <- new_model$get_block("parameter")
          new_model$add_block("proposal_parameter", lines = prior_parameter)

          prior_initial <- new_model$get_block("initial")
          new_model$add_block("proposal_initial", lines = prior_initial)

          return(new_model)
        },
        obs_to_noise = function() {
          "Copy obs variables to state variables (with '__sample_' prepended)"

          new_model <- bi_model(lines = .self$model)
          obs_block <- get_block("observation")
          obs_variables <- get_vars("obs")

          obs_var_pattern <- paste0("^(", paste(obs_variables, collapse = "|"), ")")
          state_block <- sub(obs_var_pattern, "__sample_\\1", obs_block)
          new_model$insert_lines(state_block, at_end_of = "transition")
          state_variables <- paste0("__sample_", obs_variables)
          new_model$insert_lines(paste("noise ", paste(state_variables, collapse = ", ")), after = 1)

          return(new_model)
        },
        clean_model = function() {
          ## strip comments
          model <<- sub("//.*$", "", model)

          ## remove long comments and merge lines
          i <- 1
          comment <- FALSE
          while (i <= length(model)) {
            model[i] <<- gsub("/\\*[^(\\*)]*\\*/", "", model[i])
            if (grepl("/\\*", model[i])) {
              model[i] <<- sub("/\\*.*$", "", model[i])
              comment <- TRUE
            }
            if (grepl("\\*/", model[i])) {
              model[i] <<- sub("^.*\\*/", "", model[i])
              comment <- FALSE
            }

            if (comment) {
              model <<- model[-i]
            } else {
              if (!comment && grepl("[*-+=/][[:space:]]*$", model[i])) {
                model[i] <<- paste(model[i], model[i + 1])
                model <<- model[-(i + 1)]
              } else {
                i <- i + 1
              }
            }
          }

          ## remove multiple spaces
          model <<- gsub("[[:space:]]+", " ", model)
          ## make sure there is a line break after opening braces 
          model <<- gsub("\\{(.+)$", "{\n\\1", model)
          ## make sure there is a line break before closing braces
          model <<- gsub("^(.+)\\}", "\\1\n}", model)
          ## replace semicolons with newlines
          model <<- gsub(";", "\n", model)
          ## remove trailing spaces
          model <<- gsub("[[:space:]]*$", "", model)
          ## remove initial spaces
          model <<- gsub("^[[:space:]]*", "", model)
          ## remove empty lines
          model <<- model[model!=""]

          if (length(model) > 0) {
            name <<- gsub("model ", "", gsub("\\{", "", model[1]))
            name <<- gsub("[[:space:]]*$", "", name)
            name <<- gsub("^[[:space:]]*", "", name)
          } else {
            name <<- character(0)
          }
        }, 
        get_lines = function(spaces = 2) {
          "Get the lines of a libbi model as character vectore"
          lines <- c()
          indent <- 0
          for (i in seq_along(model)) {
            if (grepl("\\}", model[i])) {
              if (indent > 0) {
                indent <- indent - 1
              } else {
                warning("There seems to be a pair of unbalanced braces")
              }
            }
            indent_spaces <- paste(rep(" ", indent * spaces), collapse = "")
            lines <- c(lines, paste0(indent_spaces, model[i]))
            if (grepl("\\{", model[i])) {
              indent <- indent + 1
            }
          }
          return(lines)
        },
        insert_lines = function(lines, before, after, at_beginning_of, at_end_of) {
          "Insert lines in a libbi model"
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
            block_lines <- .self$find_block(arg)
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
        },
        update_lines = function(num, lines) {
          "Update lines in a libbi model"
          model[num] <<- lines

          clean_model()
        },
        remove_lines = function(num) {
          "Remove lines in a libbi model"
          if (length(num) > 0) {
            if (length(grep("[\\{\\}]", model[num])) %% 2 == 1) {
              stop("Removing lines would create unbalanced braces.")
            }
            model <<- model[-num]

            clean_model()
          }
        },
        replace_all = function(old, new) {
          "Replace all instances of a string with another in a libbi model"
          model <<- gsub(old, new, model)
          clean_model()
        },
        set_name = function(name) {
          "Set model name"
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
          clean_model()
        },
        write_model_file = function(filename) {
          warning("'write_model_file' is deprecated and will be removed in version 0.7. Use 'write'.")
          .self$write(filename)
        },
        write = function(filename) {
          "Write model to file"
          if (!grepl("\\.bi$", filename)) {
            filename <- paste(filename, "bi", sep = ".")
          }
          model_name <- sub("\\.bi$", "", basename(filename))

          writeLines(.self$get_lines(), con = filename, sep = "\n")
        },
        find_block = function(name) {
          lines <- .self$model
          sub_regexp <- paste0("^[[:space:]]*(sub[[:space:]]+)?[[:space:]]*", name, "[[:space:]]*\\{")
          sub_line <- grep(sub_regexp, lines)
          if (length(sub_line) == 1) {
            lines[sub_line] <- sub(sub_regexp, "", lines[sub_line])
            open_braces <- 1
            line <- sub_line - 1
            while(open_braces > 0) {
              line <- line + 1
              braces <- grep("[\\{\\}]", lines[line], value = TRUE)
              line_brace <- 1
              open_braces <- open_braces + nchar(gsub("\\}", "", lines[line])) - nchar(gsub("\\{", "", lines[line]))
            }
            return(sub_line:line)
          } else {
            return(integer(0))
          }
        },
        get_block = function(name) {
          block <- .self$find_block(name)
          if (length(block) > 0) {
            lines <- .self$model[block]
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
        },
        remove_block = function(name) {
          block <- find_block(name)
          if (length(block) > 0) {
            model <<- .self$model[-block]
          }
        },
        add_block = function(name, lines, options) {
          .self$remove_block(name)
          model <<- c(.self$model[seq_len(length(.self$model) - 1)],
                      ifelse(missing(options), paste("sub", name,"{"),
                             paste("sub", name, paste0("(", options, ")", "{"))), 
                      paste(lines, sep = "\n"), "}", "}")
          clean_model()
        },
        get_vars = function(type, dim = FALSE, opt = FALSE) {
          line_nbs <- grep(paste0("^[[:space:]]*", type, "[[:space:]]"), .self$model)
          if (length(line_nbs) > 0) {
            ## remove qualifier
            names <- sub(paste0("^[[:space:]]*", type, "[[:space:]]"), "",
                         .self$model[line_nbs])
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
        },
        clone = function() {
          return(bi_model(lines = .self$model))
        },
        show = function() {
          if (!is.null(name)) {
            cat("bi model:", name, "\n")
            cat("==========", paste(rep("=", nchar(name)), collapse = ""), "\n",
              sep = "")
          } else {
            cat("unnamed bi model", name, "\n")
            cat("================\n")
          }
          lines <- .self$get_lines()
          if (is.null(lines)) {
            cat("// empty", "\n")
          } else {
            print(lines)
          }
        }
      )
)

`[.bi_model` = function(x, i) {
  x$get_lines()[i]
}

`[<-.bi_model` = function(x, i, value) {
  x$update_lines(i, value)
  x
}
