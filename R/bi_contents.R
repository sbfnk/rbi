#' @rdname bi_contents
#' @name bi_contents
#' @title Bi contents
#' @description
#' This function gets the name of all the variables in the passed file, list or \code{\link{libbi}} object
#'
#' @param read either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @param ... any parameters for \code{\link{bi_open}} (especially "file")
#' @return vector of variable names
#' @importFrom ncdf4 nc_close
#' @examples
#' example_output_file <- system.file(package="rbi", "example_output.nc")
#' bi_contents(example_output_file)
#' @export
bi_contents <- function(read, ...)
{
  nc <- bi_open(read, ...)
  var_names <- unname(vapply(nc[["var"]], function(x) { x[["name"]] }, ""))
  if (typeof(read) %in% c("character", "libbi")) nc_close(nc)

  return(var_names)
}
