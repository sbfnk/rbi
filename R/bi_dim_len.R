#' @rdname bi_dim_len
#' @name bi_dim_len
#' @title NetCDF dimension length
#' @description
#' This function returns the length of a dimension in a NetCDF file.
#' @param filename path to a NetCDF file
#' @return dimension length
#' @export
#' @importFrom ncdf4 nc_open nc_close
#' 
bi_dim_len <- function(filename, dim){
  ncfile <-  nc_open(tools::file_path_as_absolute(filename), verbose = FALSE)  
  len <- ncfile$dim[[which(names(ncfile$dim) == dim)]]$len
  nc_close(ncfile)
  return(len)
}
