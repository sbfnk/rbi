setwd("~/workspace/pz/results/")


file = open.ncdf("sample.nc", verbose =T)
col = "red"
name = "EPg"
nbins = 100

values = bi_read_var(file, name)
global_attributes = nc_get_attributes(ncfile)


# 
# ncfile2 = open.ncdf("adaptivefilter.nc")
# adaptiveP = bi_read_var(nc=ncfile2, name="P", ts = c(2, 54))
# close.ncdf(ncfile2)
# 
# print(length(adaptiveP))
# print(typeof(adaptiveP))


ncfile3 = open.ncdf("filter.nc")
# fixedP = bi_read_var(nc=ncfile3, name="P", ts = c(2, 54))
# dim(fixedP)
bi_read_var(nc= ncfile3, name= "time")
close.ncdf(ncfile3)



close.ncdf(ncfile2)