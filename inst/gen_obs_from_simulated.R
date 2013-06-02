gen_obs_from_simulated <- function(infile, invar, outfile, outvar, S, logn){
  #   % @itemize
  #   % @item @var{infile} Input file. Name of a NetCDF file output by sample --target prior.
  #   %
  #   % @item @var{invar} Name of variable from input file to disturb.
  #   %
  #   % @item @var{outfile} Output file. Name of a NetCDF file to create.
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
  # Open NetCDF file
  nci <- open.ncdf(infile, verbose = FALSE)
  # get desired trajectory
  onetrajectory <- as.matrix(get.var.ncdf(nci, invar))[,1]
  T <- length(onetrajectory)
  nS <- length(S)
  # create the perturbed trajectories using the standard
  # deviations specified in S
  perturbedtrajectory <- matrix(nrow=T, ncol=nS)
  if (logn) {
    for (idx in 1:nS){
      perturbedtrajectory[,idx] <- exp(log(onetrajectory) + rnorm(n=T, mean=0, sd=S[idx]))
    }
  } else {
    for (idx in 1:nS){
      perturbedtrajectory[,idx] <- onetrajectory + rnorm(n=T, mean=0, sd=S[idx])
    }  
  }
  # get the times
  times <- get.var.ncdf(nci, "time")
  close.ncdf(nci)
  # create dimensions for the new NetCDF file
  nr <- dim.def.ncdf(name=paste("nr", outvar, sep = "_"), units="", vals=1:T, create_dimvar=FALSE)
  ns <- dim.def.ncdf(name="ns", units="", vals=1:nS, create_dimvar=FALSE)
  # create variables
  ncdf_copyvar <- var.def.ncdf(invar, "", nr, -1,  prec="double")
  ncdf_outvar <- var.def.ncdf(outvar, "", list(nr, ns), -1,  prec="double")
  ncdf_time <- var.def.ncdf(paste("time", outvar, sep = "_"), "", nr, -1,  prec="double")
  # create NetCDF file
  nco <- create.ncdf(outfile, list(ncdf_outvar, ncdf_time, ncdf_copyvar))
  # fill in the variables with the values
  put.var.ncdf(nco, outvar, perturbedtrajectory, start = c(1, 1))
  put.var.ncdf(nco, paste("time", outvar, sep = "_"), times, start = 1)
  put.var.ncdf(nco, invar, onetrajectory, start = 1)
  close.ncdf(nco)
}
