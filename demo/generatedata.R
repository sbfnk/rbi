### This demo shows how to generate data from a model
### using bi simulate and then the gen_obs function provided in Rbi.

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


PathModel <- tools::file_path_as_absolute(paste(getwd(),"/../pz", sep = ""))

# Settings
settings <- bi::settings(mode = "simulate", 
args = "--model-file PZ.bi -T 100.0 -K 100 -P 10 --output-file results/datafromsimulate.nc",
                         pathModel = PathModel,
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
bi::launcher(settings)

infile <-  paste(PathModel, "/results/datafromsimulate.nc", sep = "")
outfile <- paste(PathModel, "/results/datatest.nc", sep = "")
invar <- "P"; outvar <- "P_obs"
S <- c(0.1, 1)
logn <- TRUE

gen_obs(infile, invar, outfile, outvar, S, logn)

settings <- bi::settings(mode = "sample", 
args = "--model-file PZ.bi -T 100.0 --nsamples 100 -P 128 --obs-file results/datatest.nc --output-file results/sample.nc",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
bi::launcher(settings, args = "--verbose")

