launcher <- function(settings, Args){
    if (!missing(Args)){
        settings@Args = paste(settings@Args, Args)
    }
    cdcommand <- paste("cd", settings@PathModel)
    exportPATHcommand <- paste("export PATH=", settings@PathBi, ":$PATH", sep = "")
    exportLDcommand <- paste("export LD_LIBRARY_PATH=", settings@PathLibs, ":$LD_LIBRARY_PATH", sep = "")
    exportLIBcommand <- paste("export LIBRARY_PATH=$LD_LIBRARY_PATH", sep = "")
    launchcommand <- paste("bi ", settings@Mode, " @", settings@ConfigFile, " ", settings@Args, sep = "")
    print("Launching bi with the following commands:")
    print(paste(c(cdcommand, exportPATHcommand, exportLDcommand, exportLIBcommand, launchcommand), sep = "\n"))
    command <- paste(c(cdcommand, exportPATHcommand, exportLDcommand, exportLIBcommand, launchcommand), collapse = ";")
    system(command, intern = TRUE)
    print("... bi has finished!")
}

multilauncher <- function(settings, Args = "", nruns = 1, filenamefunction = function(i) return("results/results.nc")){
  if (!missing(Args)){
    settings@Args = paste(settings@Args, Args)
  }
  for (irun in 1:nruns){
    launcher(settings, paste(Args, "--output-file", filenamefunction(irun), "--seed", irun))
  }
}
