#' @rdname libbi
#' @name libbi
#' @title LibBi Wrapper
#' @description
#' \code{libbi} allows to call \code{libbi}.
#' Upon creating a new libbi object, the following arguments can be given.
#' Once the instance is created, \code{libbi} can be run through the \code{run}
#' method documented in \code{\link{libbi_run}}.
#' 
#' @param client is either "draw", "filter", "sample"... see LibBi documentation.
#' @param model either a character vector giving the path to a model file (typically ending in ".bi"), or a \code{bi_model} object
#' @param config path to a configuration file, containing multiple arguments
#' @param global_options additional arguments to pass to the call to \code{libbi}, on top of the ones in the config file
#' @param working_folder path to a folder from which to run \code{libbi}; default to a temporary folder.
#' @param path_to_libbi path to \code{libbi} binary; by default it tries to locate \code{libbi}
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
                 "working_folder", "output_file_name", "run_flag"),
      methods = list(
        initialize = function(client, model, model_file_name,
                              config, global_options, path_to_libbi,
                              working_folder, run = FALSE, overwrite = FALSE, ...){
          result <<- list()
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
            model <<- model
          } else {
            model_file_name <<- model_file_name
            model_folder <<- dirname(model_file_name)
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
            working_folder <<- working_folder
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
            global_options <<- global_options

          if (missing(path_to_libbi)){
            # That's a bit tricky then because we really need to know where libbi is.
            # Maybe the system knows where libbi is
            path_to_libbi <<- suppressWarnings(system("which libbi", TRUE))
            if (length(.self$path_to_libbi) == 0){
              # Else try to get the path to libbi from a folder called PathToBiBin
              # created by the user with a command like 'ln -s actual_path ~/PathToBiBin'.
              path_to_libbi <<- try(tools::file_path_as_absolute("~/PathToBiBin/libbi"), TRUE)
              if (inherits(.self$path_to_libbi, "try-error")){
                # then we can try to find libbi if there's a path in the bashrc file
                bashrc <- try(system("cat ~/.bashrc", intern = TRUE), TRUE)
                if (length(bashrc) > 0){
                  # there is a bashrc file so we will read the exports from it
                  lineswithPATH <- bashrc[stringr::str_detect(bashrc, "PATH")]
                  exportPath <- lineswithPATH[stringr::str_detect(lineswithPATH, "export")]
                  exportPath <- lineswithPATH[stringr::str_sub(exportPath, start=1, end=1) != "#"]
                  exportPathcmd <- paste(exportPath, collapse=";")
                  # then we execute the export commands and try to locate libbi
                  path_to_libbi <<- system(gsub(";;", ";", 
                                               paste(exportPathcmd, "which libbi", sep = ";")),
                                          intern<-TRUE)
                }
              }
            }
          } else {
            # check that the user provided a path to an existing file
            path_to_libbi <<- tools::file_path_as_absolute(path_to_libbi)
          }
          base_command_string <<- paste(.self$path_to_libbi, .self$client,
                                        .self$config)
          if (run == TRUE) {
            .self$run(...)
          }
        },
        run = function(add_options, output_file_name, stdoutput_file_name, init, input, obs, verbose){

          if (missing(add_options))
          {
            add_options <- list()
          }

          if (!missing(input)) {
            if (is.list(input)) {
              input_file_name <- tempfile(pattern=paste0(model$name, "input"), fileext=".nc", tmpdir=working_folder)
              bi_write_file(input_file_name, input)
              add_options[["input-file"]] <- input_file_name
            } else if (is.character(input)) {
              add_options[["input-file"]] <- input_name
            } else if (class(input) == "libbi") {
              if (!input$run_flag) {
                stop("The libbi object for 'input' should be run first")
              }
              add_options[["input-file"]] <- input$result$output_file_name
            } else {
              stop("'input' must be a list, string or 'libbi' object.")
            }
          }
          
          if (!missing(init)) {
            if (is.list(init)) {
              init_file_name <- tempfile(pattern=paste0(model$name, "init"), fileext=".nc", tmpdir=working_folder)
              bi_write_file(init_file_name, init)
              add_options[["init-file"]] <- init_file_name
            } else if (is.character(init)) {
              add_options[["init-file"]] <- init_name
            } else if (class(init) == "libbi") {
              if (!init$run_flag) {
                stop("The libbi object for 'init' should be run first")
              }
              add_options[["init-file"]] <- init$result$output_file_name
            } else {
              stop("'init' must be a list, string or 'libbi' object.")
            }
          }
          
          if (!missing(obs)) {
            if (is.list(obs)) {
              obs_file_name <- tempfile(pattern=paste0(model$name, "obs"), fileext=".nc", tmpdir=working_folder)
              bi_write_file(obs_file_name, obs)
              add_options[["obs-file"]] <- obs_file_name
            } else if (is.character(obs)) {
              add_options[["obs-file"]] <- obs_name
            } else if (class(obs) == "libbi") {
              if (!obs$run_flag) {
                stop("The libbi object for 'obs' should be run first")
              }
              add_options[["obs-file"]] <- obs$result$output_file_name
            } else {
              stop("'obs' must be a list, string or 'libbi' object.")
            }
          }
          
          options <- option_string(getOption("libbi_args"), global_options, add_options)
            
          if (missing(verbose)) verbose <- FALSE
          
          if (verbose)
            options <- paste("--verbose", options)

          if (missing(output_file_name)){
            output_file_name <<- tempfile(pattern="output_file_name", fileext=".nc",
                                          tmpdir=working_folder)
          } else {
            output_file_name <<- output_file_name 
          }
          if (missing(stdoutput_file_name) && !verbose) {
            stdoutput_file_name <- tempfile(pattern="output", fileext=".txt",
                                            tmpdir=working_folder)
          }

          if (verbose) {
            stdoutput_redir_name <- ""
          } else {
            stdoutput_redir_name <- paste(">", stdoutput_file_name, "2>&1")
          }

          if (.self$model_file_name == "") {
            run_model_file <- tempfile(pattern=model$name, fileext=".bi",
                                       tmpdir=working_folder)
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
          launchcommand <- paste(.self$base_command_string, options,
                                 "--output-file", .self$output_file_name,
                                 "--model-file", rel_model_file)
          if (verbose) print("Launching LibBi with the following commands:")
          if (verbose)
            print(paste(c(cdcommand, launchcommand, stdoutput_redir_name),
                        sep = "\n"))
          command <<- paste(c(cdcommand, paste(launchcommand, stdoutput_redir_name)), collapse = ";")
#           command_dryparse <<- paste(c(cdcommand, paste(launchcommand, "--dry-parse")), collapse = ";")
          system(command, intern = TRUE)
          if (verbose) print("... LibBi has finished!")
          libbi_result <- list(output_file_name = absolute_path(filename=.self$output_file_name, 
                                                  dirname=.self$working_folder),
                               model_file_name = .self$model_file_name)
          if (!missing(stdoutput_file_name)){
            libbi_result["stdoutput_file_name"] = absolute_path(filename=stdoutput_file_name, dirname=.self$model_file_name)
          }
          run_flag <<- TRUE
          result <<- libbi_result
        },
        rerun = function(add_options, verbose){
          if (!run_flag) {
            stop("The model should be run before running 'rerun'")
          }
          
          if (missing(add_options))
          {
            add_options <- list()
          }

          options <- option_string(getOption("libbi_args"), global_options, add_options)

          if (missing(verbose)) verbose <- FALSE

          if (verbose) options <- paste("--verbose", options)

          if (.self$model_file_name == "") {
            run_model_file <- tempfile(pattern=model$name, fileext=".bi",
                                       tmpdir=working_folder)
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
          launchcommand <- paste(.self$base_command_string, options,
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
            new_wrapper <- libbi(client = .self$client,
                                      model = .self$model,
                                      config = .self$config,
                                      global_options = .self$global_options,
                                      path_to_libbi = .self$path_to_libbi,
                                      working_folder = .self$working_folder,
                                      ...)
          } else {
            new_wrapper <- libbi(client = .self$client,
                                      model = model,
                                      config = .self$config,
                                      global_options = .self$global_options,
                                      path_to_libbi = .self$path_to_libbi,
                                      working_folder = .self$working_folder,
                                      ...)
          }
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


