### This doesn't work because the PZ model is broken (not reading init.nc properly)

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "filter", configfile = "filter.conf",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" --end-time 50 --nparticles 256 --output-file results/launchPZ_PF.nc --verbose --nthreads 1")
# Have a look at the filtering distributions
resultfilename <- paste0(settings@pathModel,"/results/launchPZ_PF.nc")
logw <- getVariable(resultfilename, "logweight")
P <- getVariable(resultfilename, "P")
Z <- getVariable(resultfilename, "Z")

normaliselogweight <- function(lw){
  w <- exp(lw - max(lw))
  return(w / sum(w))
}
w = apply(X=logw, MARGIN=2, FUN=normaliselogweight)
Pmeans = apply(X = P*w, MARGIN=2, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=2, FUN=sum)
qplot(x=seq_along(Pmeans), y=Pmeans, geom = "line", col = "P") +
geom_line(aes(y=Zmeans, col = "Z")) + scale_color_discrete(name = "") +
  xlab("time") + ylab("Hidden state")

