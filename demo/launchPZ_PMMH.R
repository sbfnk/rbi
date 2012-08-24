rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "sample", configfile = "sample.conf",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" -T 50 -nsamples 50 -P 256 --output-file results/launchPZ_PMMH.nc --filter-file results/launchPZ_PMMHf.nc")
# Have a look at the posterior distribution

bi::histogram_parameter(paste(settings@pathModel,"/results/launchPZ_PMMH.nc",sep=""),variablename = "EPg")
bi::kde_parameter(paste(settings@pathModel,"/results/launchPZ_PMMH.nc",sep=""),variablename = "EPg")
bi::histogram_parameter(paste(settings@pathModel,"/results/launchPZ_PMMH.nc",sep=""),variablename = "VPg")
bi::kde_parameter(paste(settings@pathModel,"/results/launchPZ_PMMH.nc",sep=""),variablename = "VPg")
