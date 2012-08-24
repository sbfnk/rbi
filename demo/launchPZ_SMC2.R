rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
settings <- bi::settings(mode = "sample", configfile = "sample.conf", args = "--sampler smc2",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" -T 5 -nsamples 2048 -P 1024 --output-file results/launchPZ_SMC2.nc --filter-file results/launchPZ_SMC2f.nc")
# Have a look at the posterior distribution
# bi::histogram_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_SMC2.nc",sep=""),
#                         variablename = "EPg")
# bi::kde_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_SMC2.nc",sep=""),
#                   variablename = "EPg")
# bi::histogram_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_SMC2.nc",sep=""),
#                         variablename = "VPg")
# bi::kde_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_SMC2.nc",sep=""),
#                   variablename = "VPg")

bi::interactive_kde_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_SMC2.nc",sep=""),
                  variablenames = c("EPg","VPg"))
