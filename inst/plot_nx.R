plot_nx <- function(netCDFfile){
    allNx <- getParameter(netCDFfile=netCDFfile, variablename="numberx")
    medians = as.numeric((by(allNx["numberx"], allNx["TimeIndex"], FUN=function(x) median(x[,1]), simplify=TRUE)))
    qlow = as.numeric((by(allNx["numberx"], allNx["TimeIndex"], FUN=function(x) as.numeric(quantile(x[,1], probs = c(0.05))), simplify=TRUE)))
    qhigh = as.numeric((by(allNx["numberx"], allNx["TimeIndex"], FUN=function(x) as.numeric(quantile(x[,1], probs = c(0.95))), simplify=TRUE)))
    T <- max(allNx$TimeIndex)
    g <- qplot(x = 1:T, geom = "blank")
    g <- g + geom_ribbon(aes(ymin = qlow, ymax = qhigh), colour = "black")
    g <- g + geom_line(aes(y = medians), colour = "green")
    return(g)
}
