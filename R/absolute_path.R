#' @rdname absolute_path
#' @name absolute_path
#' @title Absolute Path
#' @description
#' This function is used to convert relative file paths to absolute file paths
#' without checking if the file exists as tools::file_as_absolute_path does
#'
#' @param filename name of a file, absolute or relative to a folder
#' @param dirname name of a folder where the file is supposed to be
#' @keywords internal
absolute_path <- function(filename, dirname){
  if (missing(dirname)){
    dirname <- ""
  }
  if (substr(filename, 1, 1) == "/"){
    #the filename is already absolute
    result <- normalizePath(filename, "/", FALSE)
  } else {
    if (nchar(dirname) == 0){
      result <- normalizePath(filename, "/", FALSE)
    } else {
      result <- normalizePath(paste(dirname, filename, sep = "/"), "/", FALSE)
    }
  }
  return(result)
}
