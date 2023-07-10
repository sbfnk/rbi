#' @rdname bi_dim_len
#' @name bi_dim_len
#' @title NetCDF dimension length
#' @description
#' This function returns the length of a dimension in a NetCDF file.
#' @param filename path to a NetCDF file
#' @param dim name of the dimension to check
#' @return a number, the dimension length
#' @importFrom ncdf4 nc_open nc_close
#' @keywords internal
bi_dim_len <- function(filename, dim) {
  ncfile <- nc_open(tools::file_path_as_absolute(filename), verbose = FALSE)
  if (dim %in% names(ncfile$dim)) {
    len <- ncfile$dim[[which(names(ncfile$dim) == dim)]]$len
  } else {
    len <- 0
  }
  nc_close(ncfile)
  return(len)
}
