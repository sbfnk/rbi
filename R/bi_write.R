#' @rdname bi_write
#' @name bi_write
#' @title Create (init or observation) files for LibBi
#' @description
#' This function creates an init file to specify
#' parameter values and initial conditions. This file
#' can then be passed to \code{libbi} using the \code{--init-file} option.
#'
#' @param filename a path to a NetCDF file to write the variables into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list} object, which names should be the variable names and values should be either single values, vectors of equal length, or data frames; or a single element of the type
#' @param timed if TRUE, any elements of \code{variables} that are vectors will be assumed to have a time dimension
#' @param ... arguments passed to \code{\link{netcdf_create_from_list}}
#' @return None, but creates a NetCDF file at the specified path.
#' @examples
#' filename <- tempfile(pattern="dummy", fileext=".nc")
#' a <- list(values = 1:3, dimension = "dim_a")
#' b <- list(values = 1:5, dimension = "dim_b")
#' c <- list(values = 5:9, dimension = "dim_b")
#' d <- 3
#' e <- data.frame(dim_a = rep(1:3, time = 2), dim_c = rep(1:2, each = 3), value = 1:6)
#' variables <- list(a=a, b=b, c=c, d=d, e=e)
#' bi_write(filename, variables)
#' @export
bi_write <- function(filename, variables, timed, ...){
  filename <- normalizePath(filename, "/", FALSE)
  if (!("list" %in% class(variables)) || length(variables) == 0){
    stop("please provide a non-empty list to bi_write")
  }
  vector_variables <-
    variables[sapply(variables, function(x) {is.numeric(x) && length(x) > 1})]
  variablelengths <- as.numeric(unlist(lapply(X=vector_variables, FUN=length)))
  if (!all(variablelengths == variablelengths[1])){
    stop("please provide to bi_write a list of vectors of equal length")
  }
  variables_with_dim <- variables
  for (name in names(vector_variables)){
    dim <- ifelse(!missing(timed) && timed, "time", "ns")
    vars <- data.frame(value = variables[[name]], dim = seq_along(variables[[name]]) - 1)
    names(vars)[2] <- dim
    variables_with_dim[[name]] <- vars
  }
  netcdf_create_from_list(filename, variables_with_dim, ...)
}

##' Create init files for LibBi, retained for backwards compatibility
##'
##' Users should use \code{bi_write} instead
##' @param ... parameters passed to \code{bi_write}
##' @return whatever \code{bi_write} returns
##' @seealso \code{\link{bi_write}}
bi_init_file <- function(...) {
  warning("'bi_init_file' is deprecated, use 'bi_write' instead.")
  bi_write(...)
}
