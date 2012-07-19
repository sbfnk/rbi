getParameter_ <- function(netCDFfile, variable, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  ncfile = open.ncdf(netCDFfile, verbose = verbose)
  array <- as.matrix(get.var.ncdf(ncfile, variable))
  w <- try(as.matrix(get.var.ncdf(ncfile, logweightname)))
  if (class(w) == "try-error"){
    w = matrix(1, nrow = dim(array)[1], ncol = dim(array)[2])
  } else {
    w = exp(w - max(w))
  }
  w = apply(w, 2, function(x) x / sum(x))
  if (dim(array)[2] == 1){
    array = reshape::melt.array(array, varnames = c("ParticleIndex", "TimeIndex"))
    array$TimeIndex <- length(get.var.ncdf(ncfile, "time"))
  } else {
    array = reshape::melt.array(array, varnames = c("ParticleIndex", "TimeIndex"))
  }
  names(array)[3] <- variable
  w = reshape::melt.array(w, varnames = c("ParticleIndex", "TimeIndex"))
  names(w)[3] <- "weight"
  variableDataFrame = cbind(array, w["weight"])
  close.ncdf(ncfile)
  return(variableDataFrame)
}

getParameter <- function(files, variable, timeindex, logweightname, verbose = FALSE){
  if (missing(logweightname)){
    logweightname = "logweight"
  }
  var = data.frame()
  indexrun = 0
  for (file in files){
    indexrun = indexrun + 1
    var1 <- getParameter_(file, variable, logweightname, verbose = verbose)
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
