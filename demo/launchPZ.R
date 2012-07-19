rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
settings <- bi::settings(Mode = "sample", ConfigFile = "sample.conf", Args = "--samplingmethod smc2")
#Change path to model
#Change the paths if necessary, to make sure PathBi and PathLibs are correctly set up.
settings@PathModel = "~/workspace/pz/"
print(settings)
# Once happy with the settings, launch bi.
bi::launcher(settings, Args="--output-file ~/bla.nc")
# Have a look at the posterior distributions
bi::histogram_parameter(netCDFfiles = "~/bla.nc", variable="EPg")
bi::kde_parameter(netCDFfiles = "~/bla.nc", variable="VPg")

