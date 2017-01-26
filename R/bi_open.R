#' @rdname bi_open
#' @name bi_open
#' @title Bi open
#' @description
#' This function opens an NetCDF file
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#' 
#' @param x either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @return open NetCDF connection
#' @importFrom ncdf4 nc_open
bi_open <- function(x)
{
  if (typeof(x) == "character"){
    nc <- nc_open(tools::file_path_as_absolute(x))
  } else if (class(x) == "ncdf4") {
    nc <- x
  } else if (class(x) == "libbi"){
    assert_output(x)
    nc <- nc_open(x$output_file_name)
  } else {
    stop("'read' must be a string or ncdf4.")
  }

  return(nc)
}
