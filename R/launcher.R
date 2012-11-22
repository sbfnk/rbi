launcher <- function(settings, args){
    if (!missing(args)){
        settings@args = paste(settings@args, args)
    }
    cdcommand <- paste("cd", settings@pathModel)
    exportPATHcommand <- paste("export PATH=", settings@pathBi, ":$PATH", sep = "")
#    exportLDcommand <- paste("export LD_LIBRARY_PATH=", settings@pathLibs, ":$LD_LIBRARY_PATH", sep = "")
#    exportLIBcommand <- paste("export LIBRARY_PATH=$LD_LIBRARY_PATH", sep = "")
    launchcommand <- paste("bi ", settings@mode, sep = "")
    if (settings@configfile != "")
      launchcommand <- paste(launchcommand, " @", settings@configfile, sep = "")
    launchcommand <- paste(launchcommand, settings@args, sep = " ")
    print("Launching bi with the following commands:")
    print(paste(c(cdcommand, exportPATHcommand, launchcommand), sep = "\n"))
    command <- paste(c(cdcommand, exportPATHcommand, launchcommand), collapse = ";")
    system(command, intern = TRUE)
    print("... bi has finished!")
}

multilauncher <- function(settings, args = "", nruns = 1, seeds = 1:nruns, 
                  filenamefunction = function(i) return(paste("results/results", i, ".nc", sep =""))){
  if (!missing(args)){
    settings@args = paste(settings@args, args)
  }
  for (irun in 1:nruns){
    launcher(settings, paste(args, "--output-file", filenamefunction(irun), "--seed", seeds[irun]))
  }
}
