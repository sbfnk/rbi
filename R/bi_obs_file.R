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
#' @param variable a \code{numeric} vector of observations, or a named
#'   list of such equal-length vectors with observations
#' @param times a \code{numeric} vector of times at which obserations
#'   took place (if only one variable or the observation times are
#'   equal for all variables), or a named list of such times,
#'   corresponding to each of the variables
#' @param name a string representing the name to be used in the NetCDF
#'   file (if \code{variable} is given as a vector, otherwise the
#'   names of the list elements will be used); default to "Y".
#' @return None, but creates a NetCDF file at the specified path.
#' @export
bi_obs_file <- function(filename, variable, times, name = "Y"){
  filename <- normalizePath(filename, "/", FALSE)
  if (!is.numeric(variable) & !is.list(variable)){
    stop("'variable' should be a numeric vector or a list")
  }
  if (is.list(variable)) {
    if (length(names(variable)) != length(variable)) {
      stop("if 'variable' is a list, it must be named")
    }
    if (!all(sapply(variable, is.numeric))) {
      stop("if 'variable' is a list, it must be a list of numeric vectors")
    }
    ## if (sum(duplicated(sapply(variable, length))) < length(variable) - 1) {
    ##   stop("if 'variable' is a list, its elements must be of equal length")
    ## }
  } else {
    variable <- list(variable)
    names(variable) <- name
  }

  if (missing(times)) {
    times <- lapply(variable, seq_along)
  } else if (is.numeric(times)) {
    times <- lapply(variable, function(x) {times})
  } else if (is.list(times)) {
    if (length(names(times)) != length(times)) {
      stop("if 'times' is a list, it must be named")
    }
  } else {
    stop("'times' must be numeric vector or a list")
  }

  l <- list()

  for (var.name in names(variable)) {
    l[[var.name]] <- list(values = variable[[var.name]], dimension = "nr")
    if (var.name %in% names(times)) {
      time_name <- paste("time", var.name, sep = "_")
      l[[time_name]] <- list(values = times[[var.name]], dimension = "nr")
    }
  }

  netcdf_create_from_list(filename, l)
}
