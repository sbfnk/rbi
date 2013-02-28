gen_obs <- function(settings, T, K, invar, outvar, outfile, S, logn){
  copiedsettings <- settings
  copiedsettings@mode <- "simulate"
  simfile <- paste(outfile, ".simulate", sep="")
  bi::launcher(copiedsettings, args=paste("-T", T, "-K", K, "-P 1 --output-file",
  simfile))
  gen_obs_from_simulated(simfile, invar, outfile, outvar, S, logn)
}