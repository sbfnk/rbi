### This demo shows how to generate data from a model
### using bi simulate and then the gen_obs function provided in Rbi.

## TO DO: a function that does both bi simulate and gen_obs, that'd be better

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


PathModel <- tools::file_path_as_absolute(normalizePath(paste(getwd(),"/../lg", sep = "")))

# Settings
settings <- bi::settings(mode = "simulate",
                         args = "--model-file LG.bi -T 10000.0 -K 10000 -P 1 --output-file data/obs_from_simulate.nc",
                         pathModel = PathModel,
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
# bi::launcher(settings)

# infile <-  paste(PathModel, "/data/obs_from_simulate.nc", sep = "")
outfile <- paste(PathModel, "/data/obs.nc", sep = "")
invar <- "X"; outvar <- "Y"
S <- c(0.1, 1)
logn <- FALSE

gen_obs(settings, 1210, 1210, invar, outvar, outfile, S, logn)

# gen_obs(infile, invar, outfile, outvar, S, logn)
# 
# settings <- bi::settings(mode = "sample", 
#                          args = "--model-file LG.bi -T 100.0 --nsamples 100 -P 128 --obs-file data/obs.nc --output-file results/sample.nc",
#                          pathModel = PathModel,
#                          pathBi = paste(getwd(),"/../bi/script",sep=""))
# bi::launcher(settings, args = "--verbose")
