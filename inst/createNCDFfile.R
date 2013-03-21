rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
library(ncdf)

dimension <- dim.def.ncdf(name="dimension", units="", vals=1:2, create_dimvar=FALSE)
variable <- var.def.ncdf("variable", "", dimension, -1,  prec="double")
# create NetCDF file
nco <- create.ncdf("/tmp/test.nc", list(variable))
# fill in the variables with the values
put.var.ncdf(nco, "variable", c(0.2, 0.3), start = 1)
close.ncdf(nco)



parameters <- list(a = rep(12, 324), b = rep(2033, 324))
bi_create_parameters("/tmp/test.nc", parameters)