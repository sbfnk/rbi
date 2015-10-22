#' @rdname adapt_mcmc
#' @name adapt_mcmc
#' @title Adapt the proposal distribution of MCMC using the covariance
#'   of samples
#' @description This function takes the provided \code{bi_wrapper} and
#'   runs MCMC, adapting the proposal distribution until the desired
#'   acceptance rate is achieved. If a scale is given, it will be used
#'   to adapt the proposal at each iteration
#' @param wrapper \code{bi_wrapper} (which has been run) to study
#' @param min minimum acceptance rate
#' @param max maximum acceptance rate
#' @param scale scale multiplier/divider for the proposal. If >1 this
#'   will be inverted
#' @param add_options list of additional options
#' @param samples number of samples to generate each iteration
#' @param max_iter maximum of iterations
#' @param ... parameters for bi_wrapper$run
#' @return a \code{bi_wrapper} with the desired proposal distribution
#' @export
adapt_mcmc <- function(wrapper, min = 0, max = 1, scale = 1, add_options, samples, max_iter = 10, ...) {

  if (missing(add_options)) {
    add_options <- list()
  } else if (!is.list(add_options)) {
    stop("'add_options' must be given as list.")
  }

  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  ## scale should be > 1 (it's a divider if acceptance rate is too
  ## small, multiplier if the acceptance Rate is too big)
  if (scale < 1) scale <- 1 / scale 

  model <- output_to_proposal(wrapper, scale)
  init_file <- wrapper$output_file_name
  init_np <- bi_dim_len(init_file, "np") - 1

  accRate <- 0
  if (missing(samples)) {
    if ("nsamples" %in% names(wrapper$global_options)) {
      samples <- wrapper$global_options[["nsamples"]]
    } else {
      stop("if 'nsamples' is not a global option, must provide 'samples'")
    }
  } else {
    add_options[["nsamples"]] <- samples
  }
  add_options[["init-file"]] <- init_file
  add_options[["init-np"]] <- init_np
  adapt_wrapper <-
    wrapper$clone(model = model, run = TRUE, add_options = add_options, ...)
  add_options[["init-file"]] <- adapt_wrapper$output_file_name
  add_options[["init-np"]] <- samples - 1
  iter <- 1
  adapt_scale <- 1
  while ((length(accRate) == 0 | min(accRate) < min | max(accRate) > max) && iter <= max_iter) {
    if (length(accRate) == 0 | min(accRate) < min) {
      adapt_scale <- adapt_scale / scale
    } else {
      adapt_scale <- adapt_scale * scale
    }
    cat("Adapting with scale ", adapt_scale, "\n")
    model <- output_to_proposal(adapt_wrapper, adapt_scale)
    add_options[["init-file"]] <- adapt_wrapper$output_file_name
    adapt_wrapper <-
      adapt_wrapper$clone(model = model, run = TRUE, add_options = add_options, ...)
    mcmc_obj <- mcmc(get_traces(adapt_wrapper))
    accRate <- 1 - rejectionRate(mcmc_obj)
    accRate <- accRate[accRate > 0]
    iter <- iter + 1
  }
  
  if (iter > max_iter) {
    warning("Maximum of iterations reached")
  }

  return(adapt_wrapper)
}
