#' @rdname bi_wrapper
#' @name bi_wrapper
#' @title Bi Wrapper
#' @description
#' \code{bi_wrapper} allows to call \code{libbi}.
#'
#' @param client is either "draw", "filter", "sample"... see LibBi documentation.
#' @param config path to a configuration file, containing multiple arguments
#' @param global_options additional arguments to pass to the call to \code{libbi}, on top of the ones in the config file
#' @param path_to_libbi path to the libbi binary; default to "~/PathToBiBin/libbi", so the easiest might be to create a symbolic link
#' @param path_to_model path to the model folder, from which libbi binary will be executed

#' @export bi_wrapper

bi_wrapper <- setRefClass("bi_wrapper",
      fields = c("client", "config", "global_options", "path_to_libbi", 
                 "path_to_model", "result"),
      methods = list(
        initialize = function(client, config, global_options, path_to_libbi, path_to_model,
                              result){
          result <<- list()
          if (missing(client)){
            client <<- "sample"
          } else
            client <<- client
          if (missing(path_to_model)){
            path_to_model <<- getwd()
          } else {
            path_to_model <<- absolute_path(path_to_model)
          }
          if (missing(config)){
            config <<- ""
          } else {
            config <<- absolute_path(filename=config, dirname=path_to_model)
          }
          if (missing(global_options))
            global_options <<- ""
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
            path_to_libbi <<- tools::file_path_as_absolute(.self$path_to_libbi)
          }
        },
        libbi = function(add_options, outputfile = "", stdoutputfile = ""){
          if (missing(add_options)){
            add_options <- ""
          }
          fullargs <- paste(.self$global_options, add_options)
          cdcommand <- paste("cd", .self$path_to_model)
          launchcommand <- paste(.self$path_to_libbi, .self$client)
          if (.self$config != "")
            launchcommand <- paste0(launchcommand, " @", .self$config)
          launchcommand <- paste(launchcommand, fullargs)
          if (nchar(outputfile) > 0){
            launchcommand <- paste(launchcommand, "--output-file", outputfile)
          }
          if (nchar(stdoutputfile) > 0){
            launchcommand <- paste(launchcommand, "2>", stdoutputfile)
          }
          print("Launching LibBi with the following commands:")
          print(paste(c(cdcommand, launchcommand), sep = "\n"))
          command <- paste(c(cdcommand, launchcommand), collapse = ";")
          system(command, intern = TRUE)
          print("... LibBi has finished!")
          libbi_result <- list(outputfile = absolute_path(filename=outputfile, dirname=.self$path_to_model),
                         path_to_model = .self$path_to_model)
          if (nchar(stdoutputfile) > 0){
            libbi_result["stdoutputfile"] = absolute_path(filename=stdoutputfile, dirname=.self$path_to_model)
          }
          result <<- libbi_result
        },
        show = function(){
          cat("settingsRef for LibBi\n")
          cat("* client: ", .self$client, "\n")
          cat("* config file: ", .self$config, "\n")
          cat("* additional arguments:", .self$global_options, "\n")
          cat("* path to model file:", .self$path_to_model, "\n")
          cat("* path to LibBi binary:", .self$path_to_libbi, "\n")
        }
        )
      )


