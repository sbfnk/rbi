setwd("~/workspace/pz/results/")
ncfile = open.ncdf("sample.nc", verbose =T)
ncfile
names(ncfile)
names(ncfile$dim)
close.ncdf(ncfile)

aa = nc_get_attributes(ncfile)
aa
