rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "filter", configfile = "filter.conf",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" -T 300 -P 4096 --output-file results/launchPZ_PF.nc")
# Have a look at the filtering distributions

bi::plot_filtering(filenames = paste(settings@pathModel,"/results/launchPZ_PF.nc",sep=""),
                   variablename = "P")

bi::interactive_kde_parameter(filenames = paste(settings@pathModel,"/results/launchPZ_PF.nc",sep=""),
                              variablenames = c("P","Z"))
