rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


# Settings
settings <- bi::settings(mode = "sample", configfile = "sample.conf", args = "--samplingmethod smc2")
#Change path to model
settings@pathModel = "~/workspace/pz/"
# Multiple runs of bi on the same settings but with different seeds.
nruns <- 3
filename <- function(i) paste("~/results", i, ".nc", sep = "")
multilauncher(settings, args=" -T 5 -D 256 -P 1024", nruns = nruns, 
              seeds = c(1, 10000, 23109241), filenamefunction=filename)


bi::interactive_kde_parameter(filenames = filename(1:nruns), variablenames = c("EPg", "VPg"))

