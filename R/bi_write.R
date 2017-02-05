#' @rdname bi_write
#' @name bi_write
#' @title Create (init or observation) files for LibBi
#' @description
#' This function creates a NetCDF file for LibBi from the given list of vectors
#'   and/or data frames. Since any files can be passed to \code{\link{libbi}}
#'   directly via the \code{init}, \code{input} and \code{obs} options, this is
#'   mostly used internally, this is mostly used internally.
#' @param filename a path to a NetCDF file to write the variables into, which
#'   will be overwritten if it already exists. If necessary, ".nc" will be added
#'   to the file name
#' @param variables a \code{list} object, the names of which should be the variable names and values should be either single values or data frames
#' @param timed if TRUE, any elements of \code{variables} that are vectors will be assumed to have a time dimension
#' @param ... arguments passed to \code{\link{netcdf_create_from_list}}
#' @return None, but creates a NetCDF file at the specified path.
#' @examples
#' filename <- tempfile(pattern="dummy", fileext=".nc")
#' a <- 3
#' b <- c(1, 3, 6)
#' c <- data.frame(dim_a = rep(1:3, time = 2), dim_c = rep(1:2, each = 3), value = 1:6)
#' variables <- list(a=a, b=b, c=c)
#' bi_write(filename, variables)
#' bi_file_summary(filename)
#' @export
bi_write <- function(filename, variables, timed, ...){
  filename <- normalizePath(filename, "/", FALSE)
  if (!grepl("\\.nc$", filename)) {
    filename <- paste(filename, "nc", sep = ".")
  }

  if (!("list" %in% class(variables)) || length(variables) == 0){
    stop("please provide a non-empty list to bi_write")
  }
  vector_variables <-
    variables[vapply(variables, function(x) {is.numeric(x) && length(x) > 1}, TRUE)]
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

