rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


PathModel <- tools::file_path_as_absolute(paste(getwd(),"/../pz", sep = ""))

# Settings
settings <- bi::settings(mode = "simulate", 
                         args = "--model-file PZ.bi -T 100.0 -K 100 -P 10 --output-file results/datafromsimulate.nc",
                         pathModel = paste(getwd(),"/../pz",sep=""),
                         pathBi = paste(getwd(),"/../bi/script",sep=""))
print(settings)

bi::launcher(settings)

getVariable <- function(filename, variablename, verbose = FALSE) {
  ncfile = open.ncdf(filename, verbose = verbose)
  array <- as.matrix(get.var.ncdf(ncfile, variablename))
  close.ncdf(ncfile)
  return(array)
}

gen_obs <- function(infile, invar, outfile, outvar, S, logn){
#   % @itemize
#   % @item @var{in} Input file. Name of a NetCDF file output by simulate.
#   %
#   % @item @var{invar} Name of variable from input file to disturb.
#   %
#   % @item @var{out} Output file. Name of a NetCDF file to create.
#   %
#   % @item @var{outvar} Name of variable in output file to create.
#   %
#   % @item @var{S} List of standard deviations of disturbance
#   % noise. Each value produces a corresponding record along the @t{ns}
#   % dimension of the output file.
#   %
#   % @item @var{logn} (optional) True for log-normal noise, false for normal
#   % noise.
#   %
  
  onetrajectory <- getVariable(filename=infile, variablename=invar, verbose=FALSE)[1,]
  nr <- dim.def.ncdf( "nr", "timeunit", length(onetrajectory))
  ns <- dim.def.ncdf( "ns", "unit", length(S))
  perturbedtrajectory <- matrix(nrow = length(onetrajectory), ncol=length(S))
  return(onetrajectory)  
}

x <- gen_obs(paste(PathModel, "/results/datafromsimulate.nc", sep = ""), 
        "P", paste(PathModel, "/results/datatest.nc", sep = ""),
        "P", c(0.1, 1), TRUE)
matplot(t(x), type = "l")

#create netCDF

nr <- dim.def.ncdf( "nr", "unit", 100)
ns <- dim.def.ncdf( "ns", "unit", 2)
nrobs <- dim.def.ncdf( "nr_P_obs", "unit", 100)

outvar <- var.def.ncdf("Ptilde", "count", list(nrobs, ns), -1, 
                       longname="blablabla", prec="double")

# Create a netCDF file with this variable
ncnew <- create.ncdf( paste(PathModel, "/results/datatest.nc", sep = ""), outvar )


