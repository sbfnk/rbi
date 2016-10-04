#' @rdname libbi
#' @name libbi
#' @title LibBi Wrapper
#' @description
#' \code{libbi} allows to call \code{libbi}.
#' Upon creating a new libbi object, the following arguments can be given.
#' Once the instance is created, \code{libbi} can be run through the \code{run}
#' method documented in \code{\link{libbi_run}}. Note that \code{\link{libbi}} objects can be plotted using \code{\link{plot}} if the \code{RBi.helpers} package is loaded.
#'
#' @param client is either "draw", "filter", "sample"... see LibBi documentation.
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
#'                        model = system.file(package="bi", "PZ.bi"),
#'                        global_options = list(sampler = "smc2"))
#' @seealso \code{\link{libbi_run}}
#' @export libbi
NULL 
#' @rdname libbi_run
#' @name libbi_run
#' @aliases libbi_run  bi_run  libbi
#' @title Using the LibBi wrapper to launch LibBi
#' @description
#' The method \code{run} of an instance of \code{\link{libbi}}
#' allows to launch \code{libbi} with a particular set of command line
#' arguments.
#'
#' @param add_options additional arguments to pass to the call to \code{libbi}
#' @param output_file_name path to the result file (which will be overwritten)
#' @param stdoutput_file_name path to a file to text file to report the output of \code{libbi}
#' @param init initialisation of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input)
#' @param input input of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input)
#' @param obs observations of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as observations)
#' @param verbose if TRUE, will run libbi with the '--verbose' option
#' @param ... any onrecognised options will be added to \code{add_options}
#' @return a list containing the absolute paths to the results; it is stored in the
#' \code{result} field of the instance of \code{\link{libbi}}.
#' @seealso \code{\link{libbi}}
#' @examples
#' bi_object <- libbi$new(client = "sample",
#'                        model_file_name = system.file(package="bi", "PZ.bi"),
#'                        global_options = list(sampler = "smc2"))
#' bi_object$run(add_options=list(nthreads = 1), verbose = TRUE)
#' bi_file_summary(bi_object$result$output_file_name)
NULL

libbi <- setRefClass("libbi",
      fields = c("client", "config", "global_options", "path_to_libbi",
                 "model", "model_file_name", "model_folder",
                 "base_command_string", "command", "command_dryparse", "result",
                 "working_folder", "output_file_name", "run_flag",
                 "dims"),
      methods = list(
        initialize = function(client, model, model_file_name,
                              config, global_options, path_to_libbi,
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
          if (missing(client)){
            print("you didn't provide a 'client' to libbi, it's kinda weird; default to 'sample'.")
            client <<- "sample"
          } else {
            client <<- client
          }
          if (missing(model) && missing(model_file_name)) {
              stop("you need to provide 'model', either a 'bi_model' object or a path to a valid model file in LibBi's syntax")
          }

          if (missing(model_file_name)){
            model_file_name <<- ""
            model_folder <<- ""
            if (is.character(model)) {
              model <<- bi_model(model)
            } else if ("bi_model" %in% class(model)) {
              model <<- model
            } else {
              stop("'model' must be either a 'bi_model' object or a path to a valid model file in LibBi's syntax")
            }
          } else {
            model_file_name <<- absolute_path(model_file_name)
            model_folder <<- dirname(.self$model_file_name)
            if (missing(model)) {
              model <<- bi_model(model_file_name)
            } else {
              if (file.exists(model_file_name)) {
                if (overwrite) {
                  model$write_modeL_file(model_file_name)
                } else {
                  stop("existing 'model_file_name' and 'model' given,  but overwrite is FALSE. Cowardly refusing to overwrite ", model_file_name)
                }
              } else {
                model$write_modeL_file(model_file_name)
              }
            }
          }

          if (missing(working_folder)){
            working_folder <<- tempdir()
          } else {
            working_folder <<- absolute_path(working_folder)
          }

          if (missing(config)){
            config <<- ""
          } else {
            if (model_folder == "") {
              config <<- config
            } else {
              config <<- paste0(" @", absolute_path(filename=config, dirname=model_folder))
            }
          }
          if (missing(global_options))
            global_options <<- list()
          else
            global_options <<- option_list(global_options)

          if (missing(path_to_libbi)){
            if ("path_to_libbi" %in% names(.Options)) {
              path_to_libbi <<- .Options[["path_to_libbi"]]
            } else {
              # Maybe the system knows where libbi is
              path_to_libbi <<- Sys.which("libbi")
            }
            if (length(.self$path_to_libbi) == 0){
              stop("Could not locate libbi, please either provide the path to the libbi binary via the 'path_to_libbi' option, or set the PATH to contain the directory that contains the in ~/.Renviron or set it in your R session via options(path_to_libbi = \"insert_path_here\")")
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
          base_command_string <<- paste(.self$path_to_libbi, .self$client,
                                        .self$config)

          dot_options <- list(...)
          for (option in names(dot_options))
          {
              global_options[[option]] <<- dot_options[[option]]
          }

          .self$run(from_init = TRUE, run = run, ...)
        },
        run = function(add_options, output_file_name, stdoutput_file_name, init, input, obs, time_dim, from_init = FALSE, run = TRUE, ...){

          ## if run from init, check if any of the global options are actually our option
          if (from_init) {
            for (run_option in intersect(names(match.call(expand.dots = FALSE)), names(global_options))) {
              global_options[[run_option]] <<- NULL
            }
          }

          if (missing(add_options)){
            add_options <- list()
          } else {
            add_options <- option_list(add_options)
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

          ## overwrite global and additional option, i.e. if this
          ## is run again it should use the file given here
          global_options <<- merge_by_name(global_options, file_options)

          if (run)
          {
            add_options <- merge_by_name(add_options, file_options)

            options <- option_list(getOption("libbi_args"), global_options, add_options, list(...))
            if ("end-time" %in% names(options) && !("noutputs" %in% names(options))) {
              options[["noutputs"]] <- options[["end-time"]]
            }
            opt_string <- option_string(options)
            verbose <- ("verbose" %in% names(options) && options[["verbose"]] == TRUE)

            if (missing(output_file_name)){
              output_file_name <<- tempfile(pattern=paste(.self$model$name, "output", sep = "_"),
                                            fileext=".nc",
                                            tmpdir=absolute_path(.self$working_folder))
            } else {
              output_file_name <<- absolute_path(output_file_name, getwd())
            }
            if (missing(stdoutput_file_name) && !verbose) {
              stdoutput_file_name <- tempfile(pattern="output", fileext=".txt",
                                              tmpdir=absolute_path(.self$working_folder))
            }

            if (verbose) {
              stdoutput_redir_name <- ""
            } else {
              stdoutput_redir_name <- paste(">", stdoutput_file_name, "2>&1")
            }

            if (.self$model_file_name == "") {
              run_model_file <- tempfile(pattern=.self$model$name, fileext=".bi",
                                         tmpdir=.self$working_folder)
              model$write_model_file(run_model_file)
            } else {
              run_model_file <- .self$model_file_name
            }

            if (.self$model_folder == .self$working_folder) {
              rel_model_file <- basename(run_model_file)
            } else {
              rel_model_file <- run_model_file
            }

            cdcommand <- paste("cd", .self$working_folder)
            launchcommand <- paste(.self$base_command_string, opt_string,
                                   "--output-file", .self$output_file_name,
                                   "--model-file", rel_model_file)
            if (verbose) print("Launching LibBi with the following commands:")
            if (verbose)
              print(paste(c(cdcommand, launchcommand, stdoutput_redir_name),
                          sep = "\n"))
            command <<- paste(c(cdcommand, paste(launchcommand, stdoutput_redir_name)), collapse = ";")
                                        #           command_dryparse <<- paste(c(cdcommand, paste(launchcommand, "--dry-parse")), collapse = ";")
            ret <- system(command)
            if (ret > 0) {
              if (!verbose) {
                writeLines(readLines(stdoutput_file_name))
              }
              stop("LibBi terminated with an error.")
            }
            if (verbose) print("... LibBi has finished!")
            libbi_result <-
              list(output_file_name = absolute_path(filename=.self$output_file_name, 
                                                    dirname=.self$working_folder),
                   command = launchcommand)
            if (nchar(.self$model_file_name) > 0){
              libbi_result["model_file_name"] = .self$model_file_name
            }
            if (!missing(stdoutput_file_name)){
              libbi_result["stdoutput_file_name"] = absolute_path(filename=stdoutput_file_name, dirname=.self$model_file_name)
            }
            run_flag <<- TRUE
            result <<- libbi_result
          }
        },
        rerun = function(add_options, ...){
          if (!run_flag) {
            stop("The model should be run before running 'rerun'")
          }

          if (missing(add_options))
          {
            add_options <- list()
          }

          options <- option_list(getOption("libbi_args"), global_options, add_options, list(...))
          opt_string <- option_string(option_list)
          verbose <- ("verbose" %in% names(options) && options[["verbose"]] == TRUE)

          if (missing(verbose)) verbose <- FALSE

          if (.self$model_file_name == "") {
            run_model_file <- tempfile(pattern=.self$model$name, fileext=".bi",
                                       tmpdir=.self$working_folder)
            model$write_model_file(run_model_file)
          } else {
            run_model_file <- .self$model_file_name
          }

          if (.self$model_folder == .self$working_folder) {
            rel_model_file <- basename(run_model_file)
          } else {
            rel_model_file <- run_model_file
          }

          cdcommand <- paste("cd", .self$working_folder)
          launchcommand <- paste(.self$base_command_string, opt_string,
                                 "--output-file", .self$output_file_name,
                                 "--model-file", rel_model_file)
          command_dryparse <<- paste(c(cdcommand, paste(launchcommand, "--dry-parse")), collapse = ";")
          if (verbose) print("Launching LibBi with the following commands:")
          if (verbose) print(.self$command_dryparse)
          system(.self$command_dryparse, intern = TRUE)
          if (verbose) print("... LibBi has finished!")
        },
        clone = function(model, ...) {
          if (missing(model)) {
            new_model <- .self$model$clone()
          } else
          {
            new_model <- model
          }

          new_wrapper <- libbi(client = .self$client,
                               model = new_model,
                               config = .self$config,
                               global_options = .self$global_options,
                               path_to_libbi = .self$path_to_libbi,
                               working_folder = .self$working_folder,
                               ...)
          new_wrapper$dims = .self$dims
          return(new_wrapper)
        },
        show = function(){
          cat("Wrapper around LibBi\n")
          cat("* client: ", .self$client, "\n")
          cat("* path to working folder:", .self$working_folder, "\n")
          if (.self$model_file_name != "") {
            cat("* path to model file:", .self$model_file_name, "\n")
          }
          cat("* path to LibBi binary:", .self$path_to_libbi, "\n")
        }
        )
      )


