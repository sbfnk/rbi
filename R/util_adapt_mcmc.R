#' @rdname adapt_mcmc
#' @name adapt_mcmc
#' @title Adapt the proposal distribution of MCMC using the covariance of samples
#' @description
#' This function takes the provided \code{bi_wrapper} and runs MCMC,
#' adapting the proposal distribution until the desired acceptance
#' rate is achieved
#' @param wrapper \code{bi_wrapper} (which has been run) to study
#' @param min minimum acceptance rate
#' @param max maximum acceptance rate
#' @param scale scale multiplier for the proposal
#' @param add_options list of additional options
#' @param samples number of samples to generate each iteration
#' @param ... parameters for bi_wrapper$run
#' @return a bi_model with the desired proposal distribution
#' @export
adapt_mcmc <- function(wrapper, min = 0, max = 1, scale, add_options, samples, ...) {

  if (missing(add_options)) {
    add_options <- list()
  } else if (!is.list(add_options)) {
    stop("'add_options' must be given as list.")
  }

  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  model <- output_to_proposal(wrapper, scale)
  init_file <- wrapper$output_file_name
  init_np <- bi_dim_len(init_file, "np") - 1

  accRate <- 0
  add_options[["nsamples"]] <- samples
  adapt_wrapper <-
    wrapper$clone(model = model, run = TRUE, add_options = add_options, ...)
  add_options[["init-file"]] <- adapt_wrapper$output_file_name
  add_options[["init-np"]] <- samples - 1
  while (min(accRate) < min | max(accRate) > max) {
    model <- output_to_proposal(adapt_wrapper, scale = scale)
    adapt_wrapper$run(add_options = add_options, ...)
    mcmc_obj <- mcmc(get_traces(adapt_wrapper))
    accRate <- 1 - rejectionRate(mcmc_obj)
  }
  return(model)
}
