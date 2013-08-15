#' @rdname bi_file_ncdump
#' @name bi_file_ncdump
#' @title NetCDF File Print
#' @description
#' This function prints the content of a file using the ncdump command line
#' @param filename path to a NetCDF file
#' @return None
#' @export
#' 
bi_file_ncdump <- function(filename){
  cat("Content of file", filename, "\n")
  cat(paste(system(command=paste("ncdump", filename), intern=TRUE)), collapse = "\n", sep ="\n")
}