#' @rdname bi_init_file
#' @name bi_init_file
#' @aliases bi_init_file, init_file, initfile
#' @title Bi Init File
#' @description
#' This function creates an init file to specify 
#' parameter values and initial conditions. This file
#' can then be passed to \code{libbi} using the \code{--init-file} option.
#' 
#' @param filename a path to a NetCDF file to write the variables into (will be overwritten!)
#' @param variables a \code{list} object, which names should be the variable names and values should
#' be vectors of equal length (or simply one value per key)
#' @return None, but creates a NetCDF file at the specified path.
#' @export
bi_init_file <- function(filename, variables){
  filename <- normalizePath(filename, "/", FALSE)
  if (length(variables) == 0){
    stop("please provide a non-empty list to bi_init_file")
  }
  variablelengths <- as.numeric(unlist(lapply(X=variables, FUN=length)))
  if (!all(variablelengths == variablelengths[1])){
    stop("please provide to bi_init_file a list of vectors of equal length")
  }
  nc_create_init_file_(filename, variables)
}
