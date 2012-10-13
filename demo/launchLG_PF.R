rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

source(paste(getwd(),"/demo/bootstrap.R",sep=""))
source(paste(getwd(),"/demo/LGmodel.R",sep=""))

# Settings
settings <- bi::settings(mode = "filter", configfile = "filter.conf",
                         pathModel = paste(getwd(),"/../lg",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)
verbose = FALSE
# Once happy with the settings, launch bi.
bi::launcher(settings, args=" -T 10 -P 1000 --output-file results/launchLG_PF.nc --verbose --threads 1")
# Have a look at the filtering distributions

bi::plot_filtering(filenames = paste(settings@pathModel,"/results/launchLG_PF.nc",sep=""),
                   variablename = "X")

# bi::interactive_kde_parameter(filenames = paste(settings@pathModel,"/results/launchLG_PF.nc",sep=""),
#                               variablenames = c("P","Z"))

ydata <- bi:::getVariable(paste(settings@pathModel,"/data/obs.nc",sep=""),
                 variablename = "Y")
alpha <- as.numeric(bi:::getVariable(paste(settings@pathModel,"/data/obs.nc",sep=""),
                          variablename = "A"))
beta <- as.numeric(bi:::getVariable(paste(settings@pathModel,"/data/obs.nc",sep=""),
                          variablename = "B"))
delta <- as.numeric(bi:::getVariable(paste(settings@pathModel,"/data/obs.nc",sep=""),
                          variablename = "D"))
theta <- list(alpha=alpha,vt=beta^2,vo=delta^2,v0=0,x0=0)

lg <- LGmodel(theta=theta,ys=c(NA,ydata[1:11]))
boot_out <- bootstrap_R(model=lg,N=1000)

kalman_out <- kalman(theta=theta,ys=c(NA,ydata[1:11]))

pl <- bi::plot_filtering(filenames = paste(settings@pathModel,"/results/launchLG_PF.nc",sep=""),
                         variablename = "X")

bimeans <- bi:::get_filtering_means(filenames = paste(settings@pathModel,"/results/launchLG_PF.nc",sep=""),variablename = "X")$means
bootmeans <- unlist(boot_out$means[2:12])
df <- data.frame(bimeans=bimeans,bootmeans=bootmeans,time=1:11,kmeans=kalman_out$xs[2:12])
# print(pl + geom_line(data=boot_out,aes(x=1:50,y=means[1:50])))
# plot(boot_out$means)
print(ggplot() + geom_line(data=df,aes(x=time,y=bimeans),colour="red") + geom_line(data=df,aes(x=time,y=bootmeans)) +
        geom_line(data=df,aes(x=time,y=kmeans),colour="blue"))
