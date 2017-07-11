#' @rdname bi_open
#' @name bi_open
#' @title Bi open
#' @description
#' This function opens an NetCDF file
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#'
#' @param x either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @param file file to open (out of "input", "init", "obs", "output"), if \code{x} is given as a \code{libbi} object; by default, will read output file
#' @return open NetCDF connection
#' @importFrom ncdf4 nc_open
bi_open <- function(x, file = "output")
{
  if (!missing(file) && class(x) != "libbi") {
    warning("'file' given to 'bi_open' although 'x' is not a 'libbi' object; will be ignored")
  }

  if (typeof(x) == "character"){
    nc <- nc_open(tools::file_path_as_absolute(x))
  } else if (class(x) == "ncdf4") {
    nc <- x
  } else if (class(x) == "libbi"){
    if (!missing(file)) {
      if (file == "output") {
        assert_output(x)
        filename <- x$output_file_name
      } else if (file == "obs") {
        if (!("obs-file" %in% names(x$options))) {
          stop("libbi object does not contain an 'obs' file")
        }
        filename <- x$options$`obs-file`
      } else if (file == "init") {
        if (!("init-file" %in% names(x$options))) {
          stop("libbi object does not contain an 'init' file")
        }
        filename <- x$options$`init-file`
      } else if (file == "input") {
        if (!("input-file" %in% names(x$options))) {
          stop("libbi object does not contain an 'input' file")
        }
        filename <- x$options$`input-file`
      } else {
        stop("Unknown 'file' type: ", file)
      }
    } else {
      filename <- x$output_file_name
    }
    nc <- nc_open(filename)
  } else {
    stop("'read' must be a string or ncdf4.")
  }

  return(nc)
}
