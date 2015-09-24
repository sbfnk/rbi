#' @rdname bi_obs_file
#' @name bi_obs_file
#' @aliases bi_obs_file
#' @title Create Observation Files for LibBi
#' @description
#' This function creates a NetCDF obsersation file given a numeric vector. This file
#' can then be passed to \code{libbi} using the \code{--obs-file} option.
#' @note
#' Note that it creates a time variable with indices starting from 1, and not from 0.
#' @param filename a path to a NetCDF file to write the variable into, which will be overwritten
#' if it already exists.
#' @param variable a \code{numeric} vector of observations.
#' @param times times at which obserations took place
#' @param name a string representing the name to be used in the NetCDF file; default to "Y".
#' @return None, but creates a NetCDF file at the specified path.
#' @export
bi_obs_file <- function(filename, variable, times, name = "Y"){
  filename <- normalizePath(filename, "/", FALSE)
  if (!is.numeric(variable)){
    stop("'variable' should be a numeric vector")
  }
  if (missing(times)) {
    times <- seq_along(variable)
  }
  time_name <- paste0("time_", name)
  l <- list()
  l[[name]] <- list(values = variable, dimension = "nr")
  l[[time_name]] <- list(values = times, dimension = "nr")
  netcdf_create_from_list(filename, l)
}
