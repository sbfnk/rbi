setClass("settings",
         representation(
                        mode = "character",
                        configfile = "character",
                        args = "character",
                        pathBi= "character",
                        pathModel = "character"))

setGeneric("settings", function(...) standardGeneric("settings"))
settings.constructor <- function(..., mode, configfile, args, pathBi, pathModel, pathLibs){
    if (missing(args))
        args <- ""
    if (missing(pathBi))
        pathBi <- tools::file_path_as_absolute("~/PathToBiBin/")
    if (missing(pathModel))
        pathModel <- tools::file_path_as_absolute("~/PathToPZ/")
    new("settings", mode = mode, configfile = configfile, args = args, pathBi = pathBi,
        pathModel = pathModel)
}

setMethod("settings",
          definition = function(..., mode, configfile, args, pathBi, pathModel, pathLibs){
              settings.constructor(mode = mode, configfile = configfile, args = args, 
                                    pathBi = pathBi, pathModel = pathModel)
          })

setMethod(f = "show", signature = "settings", 
          def = function(object){
            cat("Object of class ", class(object), ".\n", sep = "")
            cat("* mode: ", object@mode, "\n")
            cat("* config file: ", object@configfile, "\n")
            cat("* additional arguments:", object@args, "\n")
            cat("* (path to model file) pathModel=", object@pathModel, "\n")
            cat("* (path to bi binary) pathBi=", object@pathBi, "\n")
          })


