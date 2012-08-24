plot_filtering <- function(filenames, variablename, 
                          timeindex, adjust = 1, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  if (missing(timeindex)){
    var <- bi::getParameter(filenames, variablename, logweightname=logweightname, verbose = verbose)
    timeindex = max(var$TimeIndex)
#     var <- subset(var, TimeIndex == timeindex)
  } else {
    var <- bi::getParameter(filenames, variablename, timeindex, logweightname, verbose = verbose)
  }
  var2 <- data.frame(TimeIndex=1:timeindex,means=rep(0,timeindex),sdevs=rep(0,timeindex))
  
  for (i in 1:timeindex) {
    refac <- subset(var,TimeIndex==i)
    var2$means[i] <- weighted.mean(refac$P,refac$weight)
    var2$sdevs[i] <- sqrt(weighted.mean((refac$P)^2,refac$weight) - var2$means[i]^2)
  }
  graph <- ggplot() + geom_line(data=var2,aes(x = TimeIndex, y = means)) +
    geom_line(data=var2,aes(x = TimeIndex, y = means - sdevs), col="red") +
    geom_line(data=var2,aes(x = TimeIndex, y = means + sdevs), col="red")

#   graph <- graph + geom_density(aes_string(x = variablename, y = "..density..", weight = "weight",
#                                            colour = "Run", fill = "Run"),
#                                 adjust = adjust,
#                                 alpha = 0.5)
#   graph <- graph + ylab("density")
#   if (length(filenames) == 1){
#     graph <- graph + opts(legend.position = "none")
#   }
  return(graph)
}