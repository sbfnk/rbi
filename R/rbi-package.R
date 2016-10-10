#' \code{rbi} is an interface to \code{libbi}, a library for Bayesian Inference
#' 
#' The package includes a wrapper for the \code{libbi} script, allowing
#' to launch the \code{libbi} command from within R. 
#' It also provides various utility functions to browse the output from \code{libbi},
#' for instance to plot the results.
#' 
#' The package is made of various components:
#' \itemize{
#' \item A wrapper around \code{libbi} called \code{\link{libbi}}.
#' \item A \code{\link{bi_model}} class that can be used to load and manipulate \code{libbi} models
#' \item Functions to manipulate the results of the \code{libbi} command,
#' which are stored in NetCDF files. Those functions allow to extract variables of
#' interest.
#' }
#' 
#'
#'@name rbi-package
#'@aliases rbi RBi
#'@docType package
#'@title RBi - R interface for \code{libbi}
#'@author Pierre E. Jacob \email{pierre.jacob.work@@gmail.com}, Anthony Lee \email{awllee@gmail.com}, Lawrence Murray \email{lawrence.murray@csiro.au}, Sebastian Funk \email{sebastian.funk@@lshtm.ac.uk}
#'@references \url{http://libbi.org/}
#'@seealso \code{\link{libbi}}
#'@keywords package
#'@examples
#'
#'  demo(PZ_generate_dataset)
#'  demo(PZ_PMMH)
#'  demo(PZ_SMC2)
#'  demo(PZ_filtering)
#' example_output_file <- system.file(package="rbi", "example_output.nc")
#' bi_file_summary(example_output_file)
#' mu_sigma <- bi_read(example_output_file, c("mu", "sigma"))
#' bi_write("mu_sigma.nc", mu_sigma)
#'
#'
NULL

## set global option
.onLoad <- function(libname, pkgname) {
  libbi_args <- list()
  options(list("libbi_args" = libbi_args))
}
