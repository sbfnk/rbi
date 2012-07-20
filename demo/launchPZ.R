rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
settings <- bi::settings(mode = "sample", configfile = "sample.conf", args = "--samplingmethod smc2")
#Change path to model
#Change the paths if necessary, to make sure PathBi and PathLibs are correctly set up.
settings@pathModel = "~/workspace/pz/"
print(settings)
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" -T 5 -D 2048 -P 1024 --output-file ~/bla.nc")
# Have a look at the posterior distribution
bi::histogram_parameter(filenames = "~/bla.nc", variablename = "EPg")
bi::kde_parameter(filenames = "~/bla.nc", variablename = "VPg")


