#' @rdname libbi
#' @name libbi
#' @title LibBi Wrapper
#' @description
#' \code{libbi} allows to call \code{libbi}.
#' Upon creating a new libbi object, the following arguments can be given.
#' Once the instance is created, \code{libbi} can be run through the \code{run}
#' method documented in \code{\link{libbi_run}}. Note that \code{\link{libbi}} objects can be plotted using \code{\link{plot}} if the \code{rbi.helpers} package is loaded.
#'
#' @param model either a character vector giving the path to a model file (typically ending in ".bi"), or a \code{bi_model} object
#' @param config path to a configuration file, containing multiple arguments
#' @param global_options additional arguments to pass to the call to \code{libbi}, on top of the ones in the config file
#' @param working_folder path to a folder from which to run \code{libbi}; default to a temporary folder.
#' @param path_to_libbi path to \code{libbi} binary; by default it tries to locate \code{libbi}
#' @param input input file (given as file name or \code{libbi} object or a list of data frames
#' @param init init file (given as file name or \code{libbi} object or a list of data frames
#' @param obs observation file (given as file name or \code{libbi} object or a list of data frames
#' @param run (boolean) whether to run the model
#' using the \code{which} Unix command, after having loaded "~/.bashrc" if present;
#' if unsuccessful it tries "~/PathToBiBin/libbi"; if unsuccessful again it fails.
#' @examples
#' bi_object <- libbi$new(client = "sample",
#'                        model = system.file(package="rbi", "PZ.bi"),
#'                        global_options = list(sampler = "smc2"))
#' @seealso \code{\link{libbi_run}}, \code{\link{libbi_clone}}
#' @importFrom ncdf4 nc_open nc_close ncvar_rename
#' @export libbi
NULL
#' @rdname libbi_run
#' @name libbi_run
#' @title Using the LibBi wrapper to launch LibBi
#' @description
#' The method \code{run} of an instance of \code{\link{libbi}}
#' allows to launch \code{libbi} with a particular set of command line
#' arguments.
#'
#' @param options additional arguments to pass to the call to \code{libbi}
#' @param stdoutput_file_name path to a file to text file to report the output of \code{libbi}
#' @param init initialisation of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input)
#' @param input input of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input)
#' @param obs observations of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as observations)
#' @param time_dim The time dimension in any R objects that have been passed (\code{init}, \code{input}) and \code{obs}); if not given, will be guessed
#' @param ... any unrecognised options will be added to \code{options}
#' @seealso \code{\link{libbi}}
#' @examples
#' bi_object <- libbi$new(client = "sample",
#'                        model = system.file(package="rbi", "PZ.bi"),
#'                        global_options = list(sampler = "smc2"))
#' \dontrun{bi_object$run(options=list(nthreads = 1), verbose = TRUE)}
#' if (length(bi_object$result) > 0) {
#'   bi_file_summary(bi_object$result$output_file_name)
#' }
NULL
#' @rdname libbi_clone
#' @name libbi_clone
#' @title Clone a libbi object
#'
#' @description
#' Returns a copy of the libbi object with exactly the same properties as the original object (except for any parameters given to the \code{clone} call)
#' @param model a libbi model (or path to a model file), if the cloned libbi object is to use a different model
#' @param ... any options to the new \code{libbi} object
#' @seealso \code{\link{libbi}}
#' @examples
#' bi_object <- libbi$new(client = "sample",
#'                        model = system.file(package="rbi", "PZ.bi"),
#'                        global_options = list(sampler = "smc2"))
#' bi_object_new <- bi_object$clone()
NULL

libbi <- setRefClass("libbi",
      fields = list(client = "character",
                    config = "character",
                    global_options = "list",
                    path_to_libbi = "character",
                    model = "bi_model",
                    model_file_name = "character",
                    working_folder = "character",
                    dims = "list",
                    command = "character",
                    result = "list",
                    output_file_name = "character",
                    run_flag = "logical"),
      methods = list(
        initialize = function(model, config, global_options, path_to_libbi,
                              working_folder, dims, run = FALSE,
                              overwrite = FALSE, ...){
          result <<- list()
          libbi_dims <- list()
          if (!missing(dims)) {
            for (dim_name in names(dims))
            {
              libbi_dims[[dim_name]] <- factor(dims[[dim_name]])
            }
          }
          dims <<- libbi_dims
          run_flag <<- FALSE

          if (missing(model)) {
            model <<- NULL
          } else {
            if (is.character(model)) {
              model_file_name <<- model
              model <<- bi_model(model)
            } else if ("bi_model" %in% class(model)) {
              model <<- model
            } else {
              stop("'model' must be either a 'bi_model' object or a path to a valid model file in LibBi's syntax")
            }
          }

          if (missing(working_folder)){
            working_folder <<- tempdir()
          } else {
            working_folder <<- absolute_path(working_folder)
            if (!dir.exists(working_folder)) {
              dir.create(working_folder)
            }
          }

          if (missing(config) || config == ""){
            config <<- ""
          } else {
            config_str <- config
            if (substr(config_str, 1, 1) == "@") {
              config_str <- substr(config_str, 2, nchar(config_str))
            }
            if (file.exists(config_str)) {
              config <<- config_str
            } else {
              stop("Could not find config file ", config_str)
            }
          }

          if (missing(global_options))
            global_options <<- list()
          else
            global_options <<- option_list(global_options)

          if (!missing(path_to_libbi)) path_to_libbi <<- path_to_libbi

          return(do.call(.self$run, c(list(run_from_init = run), list(...))))
        },
        run = function(client, options, stdoutput_file_name, init, input, obs, time_dim, ...){
          "Run libbi"

          ## get hidden options 'run_from_init'; if this is passed, 'run' has
          ## been called from init and any run options have to be removed from
          ## the options that will be passed to libbi
          ## the user and removed form the dot-expanded options
          passed_options <- list(...)
          ## if run from init, check if any of the dot-expanded options assigned
          if ("run_from_init" %in% names(passed_options)) {
            run_libbi <- passed_options[["run_from_init"]]
            passed_options[["run_from_init"]] <- NULL
            for (run_option in names(passed_options)) {
              global_options[[run_option]] <<- passed_options[[run_option]]
              passed_options[[run_option]] <- NULL
            }
          } else {
            run_libbi <- TRUE
          }

          if (!missing(client)){
            client <<- client
          }

          if (missing(sample_obs)) sample_obs <- FALSE

          if (missing(options)){
            options <- list()
          } else {
            options <- option_list(options)
          }

          if (nchar(.self$config) > 0) {
            config_file_options <- paste(readLines(.self$config), collapse = " ")
          } else {
            config_file_options <- list()
          }

          ## get model
          all_options <- option_list(getOption("libbi_args"), config_file_options, global_options, options, passed_options)
          if ("model-file" %in% names(all_options)) {
            if (is.null(.self$model)) {
              model_file_name <<- absolute_path(all_options[["model-file"]], getwd())
              model <<- bi_model(model_file_name)
            } else {
              warning("'model-file' and 'model' options both provided. Will ignore 'model-file'.")
            }
          } else {
            if (is.null(.self$model)) {
              stop("A model must be provided via the 'model-file' or 'model' option.")
            } else {
              model_file_name <<- tempfile(pattern=paste(.self$model$name, "model", sep = "_"),
                                           fileext=".bi",
                                           tmpdir=absolute_path(.self$working_folder))
              model$write_model_file(.self$model_file_name)
            }
          }

          ## read file options: input, init, obs
          args <- match.call()
          file_types <- c("input", "init", "obs")
          file_args <- intersect(names(args), file_types)
          ## assign file args to global_options
          for (arg in file_args) global_options[[arg]] <<- get(arg)
          global_file_options <- intersect(names(global_options), file_types)

          file_options <- list()

          ## loop over global options that are file args
          for (file in global_file_options) {
            arg <- global_options[[file]]
            ## unset global option (we set the file option instead later)
            global_options[[file]] <<- NULL
            if (is.list(arg)) {
              arg_file_name <-
                tempfile(pattern=paste(.self$model$name, file, sep = "_"), 
                         fileext=".nc",
                         tmpdir=absolute_path(.self$working_folder))
              write_opts <- list(filename = arg_file_name,
                                 variables = arg,
                                 timed = TRUE)
              if (file == "obs") ## guess coord
              {
                write_opts[["guess_coord"]] <- TRUE
                write_opts[["guess_time"]] <- TRUE
              }
              if (!missing(time_dim)) write_opts[["time_dim"]] <- time_dim
              file_dims <- do.call(bi_write, write_opts)
              dims[names(file_dims)] <<- file_dims
              file_options[[paste(file, "file", sep = "-")]] <- arg_file_name
            } else if (is.character(arg)) {
              file_options[[paste(file, "file", sep = "-")]] <- arg
            } else if (class(arg) == "libbi") {
              if (!arg$run_flag) {
                stop("The libbi object for '", arg, "' should be run first")
              }
              file_options[[paste(file, "file", sep = "-")]] <-
                arg$result$output_file_name
            } else {
              stop("'", file, "' must be a list, string or 'libbi' object.")
            }
          }

          ## overwrite global additional option, i.e. if this
          ## is run again it should use the file given here
          global_options <<- merge_by_name(global_options, file_options)

          if (run_libbi)
          {
            if (length(.self$client) == 0) {
              message("No client provided; default to 'sample'.")
              client <<- "sample"
            }
            ## re-read options
            all_options <- option_list(getOption("libbi_args"), config_file_options,
                                       global_options, options, file_options, passed_options)
            if ("end-time" %in% names(all_options) && !("noutputs" %in% names(all_options))) {
              options[["noutputs"]] <- options[["end-time"]]
            }
            if (!("output-file" %in% names(all_options))) {
              output_file_name <<- tempfile(pattern=paste(.self$model$name, "output", sep = "_"),
                                            fileext=".nc",
                                            tmpdir=absolute_path(.self$working_folder))
            } else {
              output_file_name <<- absolute_path(all_options[["output-file"]], getwd())
            }
            all_options[["output-file"]] <- .self$output_file_name
            all_options[["model-file"]] <- .self$model_file_name
            if (sample_obs) {
              sample_model <- .self$model$obs_to_noise()
              sample_model_file_name <- tempfile(pattern=paste(.self$model$name, "model", sep = "_"),
                                                  fileext=".bi",
                                                  tmpdir=absolute_path(.self$working_folder))
              sample_model$write(sample_model_file_name)
              all_options[["model-file"]] <- sample_model_file_name
            } else {
              all_options[["model-file"]] <- .self$model_file_name
            }

            opt_string <- option_string(all_options)
            verbose <- ("verbose" %in% names(all_options) && all_options[["verbose"]] == TRUE)

            if (missing(stdoutput_file_name) && !verbose) {
              stdoutput_file_name <- tempfile(pattern="output", fileext=".txt",
                                              tmpdir=absolute_path(.self$working_folder))
            }

            if (verbose) {
              stdoutput_redir_name <- ""
            } else {
              stdoutput_redir_name <- paste(">", stdoutput_file_name, "2>&1")
            }

            if (length(path_to_libbi) == 0) {
              if (is.null(getOption("path_to_libbi"))) {
                                        # Maybe the system knows where libbi is
                path_to_libbi <<- Sys.which("libbi")
              } else {
                path_to_libbi <<- getOption("path_to_libbi")
              }
              if (nchar(.self$path_to_libbi) == 0){
                stop("Could not locate libbi, please either provide the path to the libbi binary via the 'path_to_libbi' option, or set the PATH to contain the directory that contains the binary in ~/.Renviron or set it in your R session via options(path_to_libbi = \"insert_path_here\")")
              }
            } else {
              path_to_libbi <<- path_to_libbi
            }
            if (!grepl("libbi$", .self$path_to_libbi)) {
              path_to_libbi <<- paste0(.self$path_to_libbi, "/libbi")
            }
            if (!file.exists(.self$path_to_libbi)) {
              stop("Could not find libbi executable ", path_to_libbi)
            }
            base_command_string <- paste(.self$path_to_libbi, .self$client)

            cdcommand <- paste("cd", .self$working_folder)
            launchcommand <- paste(base_command_string, opt_string)
            if (verbose) print("Launching LibBi with the following commands:")
            if (verbose)
              print(paste(c(cdcommand, launchcommand, stdoutput_redir_name),
                          sep = "\n"))
            command <<- paste(c(cdcommand, paste(launchcommand, stdoutput_redir_name)), collapse = ";")
            ret <- system(command)
            if (ret > 0) {
              if (!verbose) {
                writeLines(readLines(stdoutput_file_name))
              }
              stop("LibBi terminated with an error.")
            }
            if (verbose) print("... LibBi has finished!")
            if (sample_obs) {
              nc <- nc_open(.self$output_file_name, write=TRUE)
              for (obs_name in sample_model$get_vars("obs")) {
                ncvar_rename(nc, paste0("__sample_", obs_name), obs_name)
              }
              nc_close(nc)
            }
            libbi_result <-
              list(output_file_name = .self$output_file_name,
                   command = launchcommand,
                   options = all_options)
            if (nchar(.self$model_file_name) > 0){
              libbi_result["model_file_name"] = .self$model_file_name
            }
            if (!missing(stdoutput_file_name)){
              libbi_result["stdoutput_file_name"] = absolute_path(filename=stdoutput_file_name, dirname=getwd())
            }
            run_flag <<- TRUE
            result <<- libbi_result
          }
        },
        clone = function(client, config, global_options, path_to_libbi, model, working_folder, dims, ...) {
          "Clone a libbi object"

          new_libbi_options <- list()

          if (missing(client)) {
            new_libbi_options[["client"]] <- .self$client
          } else {
            new_libbi_options[["client"]] <- client
          }
          if (missing(config)) {
            new_libbi_options[["config"]] <- .self$config
          } else {
            new_libbi_options[["config"]] <- config
          }
          if (missing(global_options)) {
            new_libbi_options[["global_options"]] <- .self$global_options
          } else {
            new_libbi_options[["global_options"]] <- global_options
          }
          if (missing(path_to_libbi)) {
            new_libbi_options[["path_to_libbi"]] <- .self$path_to_libbi
          } else {
            new_libbi_options[["path_to_libbi"]] <- path_to_libbi
          }
          if (missing(model)) {
            new_libbi_options[["model"]] <- .self$model$clone()
          } else {
            new_libbi_options[["model"]] <- model
          }
          if (missing(working_folder)) {
            new_libbi_options[["working_folder"]] <- .self$working_folder
          } else {
            new_libbi_options[["working_folder"]] <- working_folder
          }
          if (missing(dims)) {
            new_libbi_options[["dims"]] <- .self$dims
          } else {
            new_libbi_options[["dims"]] <- dims
          }

          new_wrapper <- do.call(libbi, c(new_libbi_options, list(...)))
          return(new_wrapper)
        },
        show = function(){
          cat("Wrapper around LibBi\n")
          cat("* client: ", .self$client, "\n")
          cat("* path to working folder:", .self$working_folder, "\n")
          cat("* path to model file:", .self$model_file_name, "\n")
          if (length(.self$output_file_name) > 0) {
            cat("* path to output_file:", .self$output_file_name, "\n")
          }
        }
      )
      )
