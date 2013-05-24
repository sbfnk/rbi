rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
settings <- bi::settings(mode = "sample", configfile = "posterior.conf", args = "--sampler smc2",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" --end-time 5 --noutputs 5 -nsamples 256 --nparticles 128 --output-file results/launchPZ_SMC2.nc")
resultfilename <- paste0(settings@pathModel,"/results/launchPZ_SMC2.nc")

# Have a look at the posterior distribution
logweight <- getVariable(resultfilename, "logweight")
weight <- exp(logweight - max(logweight))
mu <- getVariable(resultfilename, "mu")
qplot(x = mu, y = ..density.., weight = weight, geom = "histogram")
sigma <- getVariable(resultfilename, "sigma")
qplot(x = sigma, y = ..density.., weight = weight, geom = "histogram")
