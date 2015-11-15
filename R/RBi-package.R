#' RBi is an interface to \code{libbi}, a library for Bayesian Inference
#' 
#' The package includes a wrapper for the \code{libbi} script, allowing
#' to launch the \code{libbi} command from within R. 
#' It also provides various utility functions to browse the output from \code{libbi},
#' for instance to plot the results.
#' 
#' The package will ultimately be made of various components.
#' \itemize{
#' \item First there is a wrapper around \code{libbi} called 
#' \code{\link{libbi}}.
#' \item Then there are funtions to manipulate the results of the \code{libbi} command,
#' which are stored in NetCDF files. Those functions allow to extract variables of
#' interest, and to plot them in various ways.
#' }
#' 
#'
#'@name RBi-package
#'@aliases Rbi RBi RBI
#'@docType package
#'@title RBi - R interface for \code{libbi}
#'@author Pierre E. Jacob \email{pierre.jacob.work@@gmail.com} Sebastian Funk \email{sebastian.funk@@lshtm.ac.uk}
#'@references \url{http://libbi.org/}
#'@seealso \code{\link{libbi}}
#'@keywords package
#'@examples
#'
#'  demo(PZ_generate_dataset)
#'  demo(PZ_PMMH)
#'  demo(PZ_SMC2)
#'  demo(PZ_filtering)
#'
NULL

## set global option
if (Sys.info()["sysname"] == "Darwin") {
  ## I don't think openmp works on OSX at the moemnt
  options(list("libbi_args" = list(openmp = FALSE)))
} else {
  options(list("libbi_args" = list()))
}

