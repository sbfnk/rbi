getParameter_ <- function(filename, variablename, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  ncfile = open.ncdf(filename, verbose = verbose)
  array <- as.matrix(get.var.ncdf(ncfile, variablename))
  data_format <- att.get.ncdf(ncfile, varid = 0, attname = "data_format")
  if (data_format$value == "PMCMC") {
    w <- matrix(1/length(array),length(array),1)
  }
  if (data_format$value == "SMC2") {
    array <- array[,1:(ncol(array)-1)]
    w <- as.matrix(get.var.ncdf(ncfile, logweightname))
    w <- w[,1:(ncol(w)-1)]
    w <- exp(w - max(w))
    w = apply(w, 2, function(x) x / sum(x))
  }
  if (data_format$value == "PF") {
    # array <- array[,1:(ncol(array)-1)]
    w <- as.matrix(get.var.ncdf(ncfile, logweightname))
    # w <- w[,1:(ncol(w)-1)]
    w <- exp(w - max(w))
    w = apply(w, 2, function(x) x / sum(x))
  }
  print(dim(array))
  if (dim(array)[2] == 1){
    array = reshape::melt.array(array, varnames = c("ParticleIndex", "TimeIndex"))
    array$TimeIndex <- length(get.var.ncdf(ncfile, "time"))
  } else {
    array = reshape::melt.array(array, varnames = c("ParticleIndex", "TimeIndex"))
  }
  names(array)[3] <- variablename
  w = reshape::melt.array(w, varnames = c("ParticleIndex", "TimeIndex"))
  names(w)[3] <- "weight"
  variableDataFrame = cbind(array, w["weight"])
  close.ncdf(ncfile)
  return(variableDataFrame)
}

getParameter <- function(filenames, variablename, timeindex, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  var = data.frame()
  indexrun = 0
  for (file in filenames){
    indexrun = indexrun + 1
    var1 <- getParameter_(file, variablename, logweightname, verbose = verbose)
    var1$Run <- indexrun
    var = rbind(var, var1)
  }
  var$Run <- factor(var$Run)
  if (missing(timeindex)){
    return(var)
  } else {
    var <- subset(var, TimeIndex == timeindex)
    return(var)
  }
}


getParameters <- function(filenames, variablenames, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  res = data.frame()
  for (indexparam in 1:length(variablenames)){
    tmpdf <- getParameter(filenames, variablenames[indexparam], logweightname=logweightname, 
                          verbose = verbose)
    names(tmpdf)[names(tmpdf) == variablenames[indexparam]] <- "ParameterValue"
    tmpdf$ParameterName <- variablenames[indexparam]
    res <- rbind(res, tmpdf)
  }
  res$ParameterName <- factor(res$ParameterName)
  return(res)
}



getEvidence <- function(files){
  Nfiles <- length(files)
  ncfile = open.ncdf(files[1])
  evidence <- get.var.ncdf(ncfile, "evidence")
  close.ncdf(ncfile)
  T <- length(evidence)
  allevidences <- matrix(ncol = Nfiles, nrow = T)
  allevidences[,1] <- cumsum(log(evidence))
  if (Nfiles > 1){
    for (ifile in 2:Nfiles){
      ncfile = open.ncdf(files[ifile])
      evidence <- get.var.ncdf(ncfile, "evidence")
      close.ncdf(ncfile)
      allevidences[,ifile] <- cumsum(log(evidence))
    }
  }
  allevidences <- reshape::melt.array(allevidences)
  names(allevidences) <- c("time", "run", "evidence")
  allevidences$run <- factor(allevidences$run)
  return(allevidences)
}
