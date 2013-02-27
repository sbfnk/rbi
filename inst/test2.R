setwd("~/workspace/pz/results/")
ncfile = open.ncdf("sample.nc", verbose =T)
ncfile
names(ncfile)
names(ncfile$dim)

bi_read_var(ncfile, "EPg")

P = get.var.ncdf(ncfile, "P")
dim(P)


ncfile2 = open.ncdf("adaptivefilter.nc", verbose = T)
adaptiveP = bi_read_var(nc=ncfile2, name="P")
close.ncdf(ncfile2)
ncfile3 = open.ncdf("filter.nc", verbose = T)
fixedP = bi_read_var(nc=ncfile3, name="P")
close.ncdf(ncfile3)

close.ncdf(ncfile)