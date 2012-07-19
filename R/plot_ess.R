plot_ess <- function(netCDFfile){
  theme_set(theme_bw())
  ncfile = open.ncdf(netCDFfile)
  ESS <- get.var.ncdf(ncfile, "ess")
  T <- length(ESS)
  acceptancerate <- get.var.ncdf(ncfile, "acceptancerate", verbose=FALSE)
  resamplingsteps <- (1:length(acceptancerate))[acceptancerate >= 0]
  graph <- qplot(x = 1:T, y = ESS, geom = "line")
  graph <- graph + ylab("ESS") + xlab("time")
  graph <- graph + geom_vline(xintercept = resamplingsteps + 1, linetype = 2) + ylim(0, max(ESS))
  close.ncdf(ncfile)
  return(graph)
}