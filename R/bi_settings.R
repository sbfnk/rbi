#' @rdname bi_settings
#' @name bi_settings
#' @title Bi Settings
#' @description
#' \code{bi_settings} is a container for libbi arguments. Once
#' an object is created, it is meant to be given to \code{\link{bi}}.
#'
#' @param client is either "draw", "filter", "sample"... see LibBi documentation.
#' @param config path to a configuration file, containing multiple arguments
#' @param args additional arguments to pass to the call to \code{libbi}, on top of the ones in the config file
#' @param path_to_libbi path to the libbi binary; default to "~/PathToBiBin/libbi", so the easiest might be to create a symbolic link
#' @param path_to_model path to the model folder, from which libbi binary will be executed

#' @exportClass bi_settings 
#' @exportMethod bi_settings

setClass("bi_settings",
         representation(
                        client = "character",
                        config = "character",
                        args = "character",
                        path_to_libbi = "character",
                        path_to_model = "character"))

setGeneric("bi_settings", function(...) standardGeneric("bi_settings"))
bi_settings.constructor <- function(..., client, config, args, path_to_libbi, path_to_model){
    if (missing(config)){
      config <- ""
    } else {
      config <- absolute_path(filename=config, dirname=path_to_model)
    }
    if (missing(args))
      args <- ""
    if (missing(path_to_libbi)){
      path_to_libbi <- absolute_path("~/PathToBiBin/libbi")
    } else {
      path_to_libbi <- absolute_path(path_to_libbi)
    }
    if (missing(path_to_model)){
      path_to_model <- getwd()
    } else {
      path_to_model <- absolute_path(path_to_model)
    }
    new("bi_settings", client = client, config = config, args = args, 
        path_to_libbi = path_to_libbi,
        path_to_model = path_to_model)
}

setMethod("bi_settings",
          definition = function(..., client, config, args, path_to_libbi, path_to_model, pathLibs){
              bi_settings.constructor(client = client, config = config, args = args, 
                                    path_to_libbi = path_to_libbi, path_to_model = path_to_model)
          })

setMethod(f = "show", signature = "bi_settings", 
          def = function(object){
            cat("Settings for LibBi.\n", sep = "")
            cat("* client: ", object@client, "\n")
            cat("* config file: ", object@config, "\n")
            cat("* additional arguments:", object@args, "\n")
            cat("* path to model file:", object@path_to_model, "\n")
            cat("* path to LibBi binary:", object@path_to_libbi, "\n")
          })


