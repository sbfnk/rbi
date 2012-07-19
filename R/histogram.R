histogram_parameter <- function(netCDFfiles, variable, 
                                 timeindex, binwidth, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  if (missing(timeindex)){
    var <- bi::getParameter(netCDFfiles, variable, logweightname=logweightname, verbose = verbose)
    timeindex = max(var$TimeIndex)
    var <- subset(var, TimeIndex == timeindex)
  } else {
    var <- bi::getParameter(netCDFfiles, variable, timeindex, logweightname, verbose = verbose)
  }
  if (missing(binwidth)){
    arrayrange = range(var[[variable]])
    arraymax = arrayrange[2]
    arraymin = arrayrange[1]
    binwidth =  (arraymax - arraymin) / 30
  }
  graph <- ggplot(var, geom = "blank")
  graph <- graph + geom_histogram(aes_string(x = variable, y = "..density..", weight = "weight",
                                             fill = "Run"),
                                  position = "identity",
                                  binwidth = binwidth,
                                  alpha = 0.5)
  graph <- graph + ylab("density")
  if (length(netCDFfiles) == 1){
    graph <- graph + opts(legend.position = "none")
  }
  return(graph)
}

kde_parameter <- function(netCDFfiles, variable, 
                                timeindex, adjust = 1, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  if (missing(timeindex)){
    var <- bi::getParameter(netCDFfiles, variable, logweightname=logweightname, verbose = verbose)
    timeindex = max(var$TimeIndex)
    var <- subset(var, TimeIndex == timeindex)
  } else {
    var <- bi::getParameter(netCDFfiles, variable, timeindex, logweightname, verbose = verbose)
  }
  graph <- ggplot(var, geom = "blank")
  graph <- graph + geom_density(aes_string(x = variable, y = "..density..", weight = "weight",
                                           colour = "Run", fill = "Run"),
                                  adjust = adjust,
                                  alpha = 0.5)
  graph <- graph + ylab("density")
  if (length(netCDFfiles) == 1){
    graph <- graph + opts(legend.position = "none")
  }
  return(graph)
}