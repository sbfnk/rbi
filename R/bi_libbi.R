#' @rdname bi_libbi
#' @name bi_libbi
#' @aliases bi_libbi libbi
#' @title R interface to libbi
#' @description
#' \code{bi_libbi} allows to launch \code{libbi} with a particular set of command line
#' arguments. This allows the user to perform the entire analysis
#' without exiting R.
#'
#' @param bi_settings a object created by a call to \code{\link{bi_settings}}
#' @param args additional arguments to pass to the call to \code{libbi}
#' @param outputfile path to the result file (which will be overwritten)
#' @param stdoutputfile path to a file to text file to report the output of \code{libbi}
#' @return a list containing the absolute paths to the results
#' @export

bi_libbi <- function(bi_settings, args, outputfile = "", stdoutputfile = ""){
    if (missing(args)){
      args <- ""
    }
    fullargs <- paste(bi_settings@args, args)
    cdcommand <- paste("cd", bi_settings@path_to_model)
    launchcommand <- paste(bi_settings@path_to_libbi, bi_settings@client)
    if (bi_settings@config != "")
      launchcommand <- paste0(launchcommand, " @", bi_settings@config)
    launchcommand <- paste(launchcommand, fullargs)
    if (nchar(outputfile) > 0){
      launchcommand <- paste(launchcommand, "--output-file", outputfile)
    }
    if (nchar(stdoutputfile) > 0){
      launchcommand <- paste(launchcommand, "2>", stdoutputfile)
    }
    print("Launching LibBi with the following commands:")
    print(paste(c(cdcommand, launchcommand), sep = "\n"))
    command <- paste(c(cdcommand, launchcommand), collapse = ";")
    system(command, intern = TRUE)
    print("... LibBi has finished!")
    result <- list(outputfile = absolute_path(filename=outputfile, dirname=bi_settings@path_to_model),
                   path_to_model = bi_settings@path_to_model)
    if (nchar(stdoutputfile) > 0){
      result["stdoutputfile"] = absolute_path(filename=stdoutputfile, dirname=bi_settings@path_to_model)
    }
    return(result)
}

