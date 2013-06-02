histogram_parameter <- function(filenames, variablename, 
                                 timeindex, binwidth, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  if (missing(timeindex)){
    var <- bi::getParameter(filenames, variablename, logweightname=logweightname, verbose = verbose)
    timeindex = max(var$TimeIndex)
    var <- subset(var, TimeIndex == timeindex)
  } else {
    var <- bi::getParameter(filenames, variablename, timeindex, logweightname, verbose = verbose)
  }
  if (missing(binwidth)){
    arrayrange = range(var[[variablename]])
    arraymax = arrayrange[2]
    arraymin = arrayrange[1]
    binwidth =  (arraymax - arraymin) / 30
  }
  graph <- ggplot(var, geom = "blank")
  graph <- graph + geom_histogram(aes_string(x = variablename, y = "..density..", weight = "weight",
                                             fill = "Run"),
                                  position = "identity",
                                  binwidth = binwidth,
                                  alpha = 0.5)
  graph <- graph + ylab("density")
  if (length(filenames) == 1){
    graph <- graph + opts(legend.position = "none")
  }
  return(graph)
}

kde_parameter <- function(filenames, variablename, 
                                timeindex, adjust = 1, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  if (missing(timeindex)){
    var <- bi::getParameter(filenames, variablename, logweightname=logweightname, verbose = verbose)
    timeindex = max(var$TimeIndex)
    var <- subset(var, TimeIndex == timeindex)
  } else {
    var <- bi::getParameter(filenames, variablename, timeindex, logweightname, verbose = verbose)
  }
  graph <- ggplot(var, geom = "blank")
  graph <- graph + geom_density(aes_string(x = variablename, y = "..density..", weight = "weight",
                                           colour = "Run", fill = "Run"),
                                  adjust = adjust,
                                  alpha = 0.5)
  graph <- graph + ylab("density")
  if (length(filenames) == 1){
    graph <- graph + opts(legend.position = "none")
  }
  return(graph)
}

interactive_kde_parameter <- function(filenames, variablenames, logweightname){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  theme_set(theme_bw())
  var <- getParameters(filenames, variablenames, logweightname=logweightname)
  xrange = range(var$ParameterValue)
  library(manipulate)
  manipulate({
    if(nextButton){
      time = time + 1
    } else if (previousButton){
      time = time - 1
    }
    graph <- ggplot(subset(var, TimeIndex == time & ParameterName == variablename), geom = "blank")
    graph <- graph + geom_density(aes_string(x = "ParameterValue", y = "..density..", weight = "weight",
                                             colour = "Run", fill = "Run"),
                                  adjust = adjust,
                                  alpha = 0.5)
    graph <- graph + xlab(paste(variablename, "- time =", time))
    graph <- graph + ylab("density")
    if (length(filenames) == 1){
      graph <- graph + opts(legend.position = "none")
    }
    graph + xlim(x.min, x.max)},
             time = slider(min = min(var$TimeIndex), max = max(var$TimeIndex), initial = max(var$TimeIndex), step = 1),
             previousButton = button("previous time"),
             nextButton = button("next time"),
             adjust = slider(min = 0.01, max = 2., initial = 1., step = 0.01),
             x.min = slider(min = xrange[1], max = xrange[2], step = diff(xrange) / 25, 
                            initial = xrange[1]),
             x.max = slider(min = xrange[1], max = xrange[2], step = diff(xrange) / 25,
                            initial = xrange[2]),
             variablename = picker(as.list(variablenames), initial = variablenames[1], label="variablename")
  )
}
