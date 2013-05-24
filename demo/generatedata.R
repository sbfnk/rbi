### This demo shows how to generate data from a model
### using bi sample --target prior and then the gen_obs function provided in Rbi.

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


PathModel <- tools::file_path_as_absolute(normalizePath(paste(getwd(),"/../lg", sep = "")))

# Settings
settings <- bi::settings(mode = "sample --target prior",
                         args = "--model-file LG.bi --end-time 10000.0 --noutputs 10000 --nparticles 1 --output-file data/obs_from_sample --target prior.nc",
                         pathModel = PathModel,
                         pathBi = paste(getwd(),"/../bi/script",sep=""))

# infile <-  paste(PathModel, "/data/obs_from_sample --target prior.nc", sep = "")
outfile <- paste(PathModel, "/data/obs.nc", sep = "")
invar <- "X"; outvar <- "Y"
S <- c(0.1, 1)
logn <- FALSE

gen_obs(settings, 100, 100, invar, outvar, outfile, S, logn)

