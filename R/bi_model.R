#' @rdname bi_model
#' @name bi_model
#' @title Bi Model
#' @description
#' \code{bi_model} creates a model object for \code{Rbi} from a libbi file.
#' Once the instance is created, the model can either be fed to a \code{bi_wrapper}
#' object.
#' 
#' @param filename is the file name of the model file
#' @examples
#' bi_sir <- bi_model$new(filename = "sir.bi")
#' @seealso \code{\link{bi_model_fix_noise}}, \code{\link{bi_model_propose_prior}}, \code{\link{insert_lines}}, \code{\link{update_lines}}, \code{\link{delet_lines}}, \code{\link{write_model_file}}, 
#' #' @export bi_model
NULL 
#' @rdname bi_model_fix_noise
#' @name bi_model_fix_noise
#' @title Fix noise term of a libbi model
#' @description
#' Replaces all noises with fixed values as given in 'fixed', otherwise as given in 'default'
#'
#' @param fixed a vector of values to be assigned to the (usually) noise value
#' @param default an optional default value to give to all noises not specified in \code{fixed}
#' @return a bi model object of the new model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="bi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$fix_noise(fixed = c(alpha = 0))
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
#' @rdname bi_model_insert
#' @name bi_model_insert
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
#' PZ$insert(lines = "noise beta", after = 8)
NULL 
#' @rdname bi_model_update
#' @name bi_model_update
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
#' PZ$update(23, "alpha ~ normal(mu, sigma)")
NULL 
#' @rdname bi_model_delete
#' @name bi_model_delete
#' @title Delete line(s) in a libbi model
#' @description
#' Deletes one or more lines in a libbi model.
#'
#' @param num line number(s) to delete
#' @return the updated bi model
#' @seealso \code{\link{bi_model}}
#' @examples
#' model_file_name <- system.file(package="bi", "PZ.bi")
#' PZ <- bi_model(filename = model_file_name)
#' PZ$delete(2)
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
        fix_noise = function(fixed, default){
          if (missing(fixed) && missing(default)) {
            stop("At least one of 'fixed' and 'default' must be given.")
          }
          if (missing(fixed)) {
            fixed <- list()
          }
            
          fix_model <- model

          noise_line_nbs <- grep("^[[:space:]]*noise[[:space:]]", fix_model)
          indent <- sub("^([[:space:]]*).*$", "\\1", fix_model[noise_line_nbs[1]])

          if (length(noise_line_nbs) > 0) {
            ## remove "noise" term
            noise_vec <- .self$get_vars("noise")

            unmatched_names <- setdiff(names(fixed), noise_vec)
            if (length(unmatched_names) > 0) {
              stop("Given noises ", paste(unmatched_names, collapse = ", "),
                   " not found in model.")
            }

            fix_model <- fix_model[-noise_line_nbs]
            for (noise in noise_vec) {
              if (!missing(default) || (noise %in% names(fixed))) {
                fixed_lines <- c()
                if (!(noise %in% names(fixed))) {
                  fixed[[noise]] <- default
                }
                fixed_lines <- c(fixed_lines,
                                 paste0(indent, "const ", noise, " = ",
                                        fixed[[noise]]))
                fix_model <- c(fix_model[1:(noise_line_nbs[1] - 1)],
                               fixed_lines, 
                               fix_model[(noise_line_nbs[1]):length(fix_model)])
                
                this_noise_assignment <-
                  grep(paste0(noise, "[[:space:]]*\\[[^]]*\\][[:space:]]*~"),
                       fix_model)
                if (length(this_noise_assignment) > 0) {
                  fix_model <- fix_model[-this_noise_assignment]
                }

                ## remove dimensions
                fix_model <-
                  sub(paste0(noise, "[[:space:]]*\\[[^]]*\\]"), noise, fix_model)
              }
            }
          }

          return(bi_model(lines = fix_model))
        },
        propose_prior = function() {
          new_model <- bi_model(lines = .self$model)

          ## remove parameter proposal
          prior_parameter <- new_model$get_block("parameter")
          new_model$add_block("propose_parameter", lines = prior_parameter)

          prior_initial <- new_model$get_block("initial")
          new_model$add_block("propose_initial", lines = prior_initial)

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
        insert = function(before, after, lines) {
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
        update = function(num, lines) {
          model[num] <<- lines

          clean_model()
        },
        delete = function(num) {
          if (length(grep("[\\{\\}]", model[num])) %% 2 == 1) {
            stop("Delete would create unbalanced braces.")
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
              while (open_braces > 0 & line_brace <= length(braces)) {
                if (braces[line_brace] == "{")
                  open_braces <- open_braces + 1
                else (braces[line_brace] == "}")
                  open_braces <- open_braces - 1
              }
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
        add_block = function(name, options, lines) {
          .self$remove_block(name)
          model <<- c(.self$model[seq_len(length(.self$model) - 1)],
                      ifelse(missing(options), paste("sub", name,"{"),
                             paste("sub", name, paste0("(", options, ")", "{"))), 
                      lines, "}", "}")
        },
        get_vars = function(type) {
          line_nbs <- grep(paste0("^[[:space:]]*", type, "[[:space:]]"), .self$model)
          if (length(line_nbs) > 0) {
            ## remove "noise" term
            names <- sub(paste0("^[[:space:]]*", type, "[[:space:]]"), "",
                         .self$model[line_nbs])
            ## remove dimensions
            names <- sub("\\[.*\\]", "", names)
            names_vec <- unlist(strsplit(names, ","))
            return(names_vec)
          } else {
            return(NA)
          }
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
