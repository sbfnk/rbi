#' @rdname bi_open
#' @name bi_open
#' @title Bi open
#' @description
#' This function opens an NetCDF file
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#' 
#' @param read either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @return open NetCDF connection
#' @importFrom ncdf4 nc_open
bi_open <- function(read)
{
  if (typeof(read) == "character"){
    nc <- nc_open(tools::file_path_as_absolute(read))
  } else if (class(read) == "ncdf4") {
    nc <- read
  } else if (class(read) == "libbi"){
    if (!read$run_flag) {
      stop("The libbi object should be run first")
    }
    filename <- tools::file_path_as_absolute(read$output_file_name)
    if (read$timestamp < file.mtime(filename)) {
      stop("Output file ", filename, " has been modified since LibBi was run.")
    }
    nc <- nc_open(filename)
  } else {
    stop("'read' must be a string, ncdf4 or libbi object.")
  }

  return(nc)
}
