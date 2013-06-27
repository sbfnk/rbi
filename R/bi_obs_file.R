#' @rdname bi_obs_file
#' @name bi_obs_file
#' @aliases bi_obs_file, obs_file, initfile
#' @title Bi Obs File
#' @description
#' This function creates an observation file. This file
#' can then be passed to \code{libbi} using the \code{--obs-file} option.
#' 
#' @param filename a path to a NetCDF file to write the variable into (will be overwritten!)
#' @param variable a \code{numeric} vector.
#' @param name a string representing the name to be used in the NetCDF file; default to "Y".
#' @return None, but creates a NetCDF file at the specified path.
#' @export
bi_obs_file <- function(filename, variable, name = "Y"){
  filename <- normalizePath(filename, "/", FALSE)
  if (class(variable) != "numeric"){
    stop("'variable' should be a numeric vector")
  }
  time_sequence <- seq_along(variable)
  time_name <- paste0("time_", name)
  l <- list()
  l[[name]] <- variable
  l[[time_name]] <- time_sequence
  nc_create_obs_file_(filename, paste0("nr_", name), l)
}
