#' @rdname acceptance_rate
#' @name acceptance_rate
#' @title Compute acceptance rate
#' @description
#' This function takes the provided \code{\link{libbi}} object which has been
#' run, or a bi file, and returns a the acceptance rate
#' @param ... parameters to \link{\code{get_traces}} (especially 'run')
#' @return acceptance rate
#' @export
acceptance_rate <- function( ...) {

  mcmc_obj <- mcmc(get_traces(all = TRUE, ...))
  accRate <- max(1 - rejectionRate(mcmc_obj))
  
  return(accRate)
}
