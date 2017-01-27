#' @rdname bi_file_summary
#' @name bi_file_summary
#' @title NetCDF File Summary
#' @description
#' This function prints a little summary of the content
#' of a NetCDF file, as well as its creation time. You can
#' then retrieve variables of interest using \code{\link{bi_read}}.
#' @param x A \code{\link{libbi}} object (in which case the output file will be taken) or a path to a NetCDF file
#' @return None
#' @export
#' @importFrom ncdf4 nc_open nc_close
#' 
bi_file_summary <- function(x){
  if ("libbi" %in% class(x)) {
    assert_output(x)
    filename <- x$output_file_name
    message("Summary of libbi output file")
  } else {
    filename <- x
  }
  print(file.info(tools::file_path_as_absolute(filename))[,c("mtime")])
  ncfile <- nc_open(tools::file_path_as_absolute(filename), verbose = FALSE)
  print(ncfile)
  nc_close(ncfile)
}
