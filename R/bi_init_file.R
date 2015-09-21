#' @rdname bi_init_file
#' @name bi_init_file
#' @aliases bi_init_file
#' @title Create Init Files for LibBi
#' @description
#' This function creates an init file to specify 
#' parameter values and initial conditions. This file
#' can then be passed to \code{libbi} using the \code{--init-file} option.
#' 
#' @param filename a path to a NetCDF file to write the variables into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list} object, which names should be the variable names and values should
#' be vectors of equal length (or simply one value per key)
#' @return None, but creates a NetCDF file at the specified path.
#' @export
bi_init_file <- function(filename, variables){
  filename <- normalizePath(filename, "/", FALSE)
  if (length(variables) == 0){
    stop("please provide a non-empty list to bi_init_file")
  }
  variablelengths <- as.numeric(unlist(lapply(X=variables, FUN=length)))
  if (!all(variablelengths == variablelengths[1])){
    stop("please provide to bi_init_file a list of vectors of equal length")
  }
  variables_with_dim <- list()
  for (name in names(variables)){
    variables_with_dim[[name]] <- list(values = variables[[name]], dimension = "ns")
  }
  netcdf_create_from_list(filename, variables_with_dim)
}
