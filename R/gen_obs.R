gen_obs <- function(settings, T, K, invar, outvar, outfile, S, logn){
  copiedsettings <- settings
  copiedsettings@mode <- "sample --target prior"
  simfile <- paste(outfile, ".simulated", sep="")
  bi::launcher(copiedsettings, args=paste("--end-time", T, "--noutputs", K, "--nparticles 1 --output-file",
  simfile))
  gen_obs_from_simulated(simfile, invar, outfile, outvar, S, logn)
}