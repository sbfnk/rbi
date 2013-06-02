gen_obs <- function(bi_settings, T, K, invar, outvar, outfile, S, logn){
  copiedbi_settings <- bi_settings
  copiedbi_settings@client <- "sample --target prior"
  simfile <- paste(outfile, ".simulated", sep="")
  bi(copiedbi_settings, args=paste("--end-time", T, "--noutputs", K, "--nparticles 1 --output-file",
  simfile))
  gen_obs_from_simulated(simfile, invar, outfile, outvar, S, logn)
}