#' @rdname acceptance_rate
#' @name acceptance_rate
#' @title Compute acceptance rate
#' @description
#' This function takes the provided \code{bi_wrapper} which has been
#' run and returns a the acceptance rate
#' @param wrapper a \code{bi_wrapper} which has been run
#' @param ... parameters to \code{get_traces} (e.g., dimensions)
#' @return 
#' @export
acceptance_rate <- function(wrapper, ...) {

  mcmc_obj <- mcmc(get_traces(wrapper, all = TRUE, ...))
  accRate <- max(1 - rejectionRate(mcmc_obj))
  
  return(accRate)
}
