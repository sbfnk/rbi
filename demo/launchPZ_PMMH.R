rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "sample", configfile = "posterior.conf",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" --end-time 50 -nsamples 50 --nparticles 128 --output-file results/launchPZ_PMMH.nc")
resultfilename <- paste0(settings@pathModel,"/results/launchPZ_PMMH.nc")

# Have a look at the posterior distribution
mu <- getVariable(resultfilename, "mu")
qplot(x = mu, y = ..density.., geom = "histogram")
sigma <- getVariable(resultfilename, "sigma")
qplot(x = sigma, y = ..density.., geom = "histogram")
