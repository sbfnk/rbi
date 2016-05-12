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
#' bi_sir <- bi_model$new(filename = "sir.bi")
#' @seealso \code{\link{bi_model_fix}}, \code{\link{bi_model_propose_prior}}, \code{\link{bi_model_insert_lines}}, \code{\link{bi_model_update_lines}}, \code{\link{bi_model_remove_lines}}, \code{\link{bi_model_write_model_file}}, 
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
#' model_file_name <- system.file(package="bi", "PZ.bi")
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
#' model_file_name <- system.file(package="bi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$propose_prior()
NULL 
#' @rdname bi_model_insert_lines
#' @name bi_model_insert_lines
#' @title Insert lines in a libbi model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or \code{after} is given, the line(s) will be inserted before or after a given line number, respectively. If neither is given, the line(s) will be added at the end.
#'
#' @param before line number before which to insert line(s)
#' @param after line number after which to insert line(s)
#' @param lines vector or line(s)
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="bi", "PZ.bi")
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
#' @param lines vector of iline(s)
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="bi", "PZ.bi")
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
#' model_file_name <- system.file(package="bi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$remove_lines(2)
NULL 
#' @rdname bi_model_write_model_file
#' @name bi_model_write_model_file
#' @title Writes a bi model to a file.
#' @description
#' Writes a bi model to a file given by \code{filename}. The extension '.bi' will be added if necessary.
#'
#' @param filename name of the file to be written
#' @return the return value of the \code{\link{writeLines}} call.
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="bi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$write_model_file("PZ")
NULL 

bi_model <- setRefClass("bi_model",
      fields = c("model", "name"),
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
            warning("Neither 'filename' or 'lines' has been given. ",
                    "Will initialise an empty model")
            model <<- c()
          }

          clean_model()
        },
        fix = function(...){

            fix_model <- model

            fixed = list(...)

            ## variables that are to be fixed
            var_str <-
                paste0("^[[:space:]]*(noise|param|state|const)[[:space:]]+(",
                       paste(names(fixed), collapse = "|"), ")[[:space:]$[]")
            var_line_nbs <- grep(var_str, fix_model)

            var_vec <- c(.self$get_vars("noise"),
                         .self$get_vars("param"),
                         .self$get_vars("state"),
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
                    first_const_line <- grep("^[[:space:]]*(noise|param|state|const)[[:space:]]+", fix_model)[1]
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
                            "[[:space:]]*(\\[[^]]*\\])?[[:space:]]*(~|=|<-)"),
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

            return(bi_model(lines = fix_model))
        },
        propose_prior = function() {
          new_model <- bi_model(lines = .self$model)

          ## remove parameter proposal
          prior_parameter <- new_model$get_block("parameter")
          new_model$add_block("proposal_parameter", lines = prior_parameter)

          prior_initial <- new_model$get_block("initial")
          new_model$add_block("proposal_initial", lines = prior_initial)

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
            name <<- NULL
          }
        }, 
        get_lines = function(spaces = 2) {
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
        insert_lines = function(lines, before, after) {
          if (!missing(before)) {
            if (!missing(after)) {
              stop("Must give at most one of 'before' or 'after'")
            } else {
              after <- before - 1
            }
          }

          if (after > length(model)) {
            stop("model only has ", length(model), " lines, higher requested.")
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
          model[num] <<- lines

          clean_model()
        },
        remove_lines = function(num) {
          if (length(grep("[\\{\\}]", model[num])) %% 2 == 1) {
            stop("Removing lines would create unbalanced braces.")
          }
          model <<- model[-num]

          clean_model()
        },
        set_name = function(name) {
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
          if (!grepl("\\.bi$", filename)) {
            filename <- paste(filename, "bi", sep = ".")
          }
          model_name <- sub("\\.bi$", "", basename(filename))
          
          writeLines(.self$get_lines(), con = filename, sep = "\n")
        },
        find_block = function(name) {
          lines <- .self$model
          sub_line <-
            grep(paste0("[[:space:]]*sub[[:space:]]+", name, "[[:space:]]*\\{"),
                 lines)
          if (length(sub_line) == 1) {
            lines[sub_line] <- sub(paste0("[[:space:]]*sub[[:space:]]+", name,
                                          "[[:space:]]*\\{"), "", 
                                   lines[sub_line])
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
              sub(paste0("^[[:space:]]*sub[[:space:]]+", name, "[[:space:]]*\\{"),
                  "", lines[1])
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
            names <- sub("[[:space:]]", "", names)
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
