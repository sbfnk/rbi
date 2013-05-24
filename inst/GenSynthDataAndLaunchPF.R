### This doesn't work because the PZ model is broken (not reading init.nc properly)

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

pathToModel <- "/home/pierre/workspace/pz/"
pathToBi <- "/home/pierre/workspace/bi/script/"
modelFile <- "PZ.bi"
initfile <- "/tmp/someinitfile.nc"
TimeHorizon <- 1000
parameters <- list(EPg = 0.3, VPg = 0.1)

bi_create_parameters(initfile, parameters)

# Settings
fsettings <- bi::settings(mode = "filter", configfile = "filter.conf",
                         pathModel = pathToModel,
                         pathBi = pathToBi, 
                         args = paste("--init-file", initfile))

obsfile <- tempfile(pattern = "obs", fileext = ".nc")
gensettings <- bi::settings(mode = "sample --target prior",
                            args = paste("--model-file", modelFile,
                            "--seed ", 123, "--init-file", initfile), pathModel = pathToModel, 
                            pathBi = pathToBi)
invar <- "P"; outvar <- "P_obs"
S <- c(0.1); logn <- TRUE

bi::gen_obs(gensettings, TimeHorizon, TimeHorizon, invar, outvar, obsfile, S, logn)

print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
fargs <- paste0("--end-time ", TimeHorizon, " --obs-file ", obsfile, 
               " --nparticles 256 --output-file results/launchPZ_PF.nc --verbose --nthreads 1" )
bi::launcher(fsettings, args=fargs)
