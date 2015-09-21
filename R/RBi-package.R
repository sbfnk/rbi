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
#' \code{\link{bi_wrapper}}.
#' \item Then there are funtions to manipulate the results of the \code{libbi} command,
#' which are stored in NetCDF files. Those functions allow to extract variables of
#' interest, and to plot them in various ways.
#' }
#' 
#'
#'@name RBi-package
#'@aliases Rbi RBi RBI
#'@docType package
#'@title RBi - R nterface for \code{libbi}
#'@author Pierre E. Jacob <pierre.jacob.work@@gmail.com>
#'@references \url{http://libbi.org/}
#'@seealso \code{\link{bi_wrapper}}
#'@keywords package
#'@examples
#'
#'  demo(LG_filtering.R)
#'  demo(PZ_PMMH.R)
#'
NULL
