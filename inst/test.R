rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


PathModel <- tools::file_path_as_absolute(paste(getwd(),"/../pz", sep = ""))

# Settings
settings <- bi::settings(mode = "sample --target prior", 
args = "--model-file PZ.bi --end-time 100.0 --noutputs 100 --nparticles 10 --output-file results/datafromsample --target prior.nc",
                         pathModel = PathModel,
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
bi::launcher(settings)

infile <-  paste(PathModel, "/results/datafromsample --target prior.nc", sep = "")
outfile <- paste(PathModel, "/results/datatest.nc", sep = "")
invar <- "P"; outvar <- "P_obs"
S <- c(0.1, 1)
logn <- TRUE

gen_obs(infile, invar, outfile, outvar, S, logn)

settings <- bi::settings(mode = "sample", 
args = "--model-file PZ.bi --end-time 100.0 --nsamples 100 --nparticles 128 --obs-file results/datatest.nc --output-file results/sample.nc",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
bi::launcher(settings, args = "--verbose")

