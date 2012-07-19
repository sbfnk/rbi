setClass("settings",
         representation(
                        Mode = "character",
                        ConfigFile = "character",
                        Args = "character",
                        PathBi= "character",
                        PathModel = "character",
                        PathLibs = "character"))

setGeneric("settings", function(...) standardGeneric("settings"))
settings.constructor <- function(..., Mode, ConfigFile, Args, PathBi, PathModel, PathLibs){
    if (missing(Args))
        Args <- ""
    if (missing(PathBi))
        PathBi <- tools::file_path_as_absolute("~/PathToBiBin/")
    if (missing(PathModel))
        PathModel <- tools::file_path_as_absolute("~/PathToPZ/")
    if (missing(PathLibs))
        PathLibs <- tools::file_path_as_absolute("~/PathToBiLibs/")
    new("settings", Mode = Mode, ConfigFile = ConfigFile, Args = Args, PathBi = PathBi,
        PathModel = PathModel, PathLibs = PathLibs)
}

setMethod("settings",
          definition = function(..., Mode, ConfigFile, Args, PathBi, PathModel, PathLibs){
              settings.constructor(Mode = Mode, ConfigFile = ConfigFile, Args = Args, 
                                    PathBi = PathBi, PathModel = PathModel, PathLibs = PathLibs)
          })

setMethod(f = "show", signature = "settings", 
          def = function(object){
            cat("Object of class ", class(object), ".\n", sep = "")
            cat("* Mode: ", object@Mode, "\n")
            cat("* Config file: ", object@ConfigFile, "\n")
            cat("* Additional arguments:", object@Args, "\n")
            cat("* (path to model file) PathModel=", object@PathModel, "\n")
            cat("* (path to bi binary) PathBi=", object@PathBi, "\n")
            cat("* (path to bi libs) PathLibs=", object@PathLibs, "\n")
          })


