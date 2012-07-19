printVariables <- function(netCDFfile){
  ncfile = open.ncdf(netCDFfile, verbose = FALSE)
  print(ncfile)
  close.ncdf(ncfile)
}
