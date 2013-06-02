plot_acceptrate <- function(netCDFfile){
  theme_set(theme_bw())
  ncfile = open.ncdf(netCDFfile)
  ESS <- get.var.ncdf(ncfile, "ess")
  acceptancerate <- get.var.ncdf(ncfile, "acceptancerate")
  print(acceptancerate)
  
  T <- length(ESS)
  resamplingSteps <- get.var.ncdf(ncfile, "resamplingsteps") + 1
  print(resamplingSteps)
  graph <- qplot(x = resamplingSteps, y = acceptancerate , geom = "line")
  graph <- graph + geom_point(size = 4) 
  graph <- graph + ylab("acceptance rate ") + xlab("time") + xlim(0, T) + ylim(0, 1)
  close.ncdf(ncfile)
  return(graph)
}
