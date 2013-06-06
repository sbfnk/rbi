#' @rdname bi_file_ncdump
#' @name bi_file_ncdump
#' @title Bi File ncdump
#' @description
#' This function prints the content of a file using the ncdump command line
#' @param filename path to a NetCDF file
#' @return None
#' @export
#' 
bi_file_ncdump <- function(ncdf_file){
  cat(paste(system(command=paste("ncdump", ncdf_file), intern=TRUE)), collapse = "\n", sep ="\n")
}