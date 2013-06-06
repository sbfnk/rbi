#' @rdname bi_file_summary
#' @name bi_file_summary
#' @title Bi File Summary
#' @description
#' This function prints a little summary of the content
#' of a NetCDF file, as well as its creation time. You can
#' then retrieve variables of interest using \code{\link{bi_read_var}}.
#' @param filename path to a NetCDF file
#' @return None
#' @export
#' 
bi_file_summary <- function(filename){
  print("File summary")
  print(file.info(tools::file_path_as_absolute(filename))[,c("mtime")])
  ncfile = open.ncdf(tools::file_path_as_absolute(filename), verbose = FALSE)
  print(ncfile)
  close.ncdf(ncfile)
}
