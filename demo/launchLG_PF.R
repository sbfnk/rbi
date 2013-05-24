rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "filter", configfile = "filter.conf",
                         pathModel = paste(getwd(),"/../lg",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" --end-time 100 --nparticles 1000 --output-file results/launchLG_PF.nc --verbose --nthreads 1")

resultfilename <- paste0(settings@pathModel,"/results/launchLG_PF.nc")
logw <- getVariable(resultfilename, "logweight")
X <- getVariable(resultfilename, "X")

normaliselogweight <- function(lw){
  w <- exp(lw - max(lw))
  return(w / sum(w))
}
w = apply(X=logw, MARGIN=2, FUN=normaliselogweight)
Xmeans = apply(X = X*w, MARGIN=2, FUN=sum)
qplot(x=seq_along(Xmeans), y=Xmeans, geom = "line", col = "X") +
  scale_color_discrete(name = "") +
  xlab("time") + ylab("Hidden state")
