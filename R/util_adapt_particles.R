#' @rdname adapt_particles
#' @name adapt_particles
#' @title Adapt the number of particles 
#' @description This function takes the provided \code{bi_wrapper} and
#'   runs MCMC at a single point (i.e., repeatedly proposing the same paramters),
#'   adapting the number of particles distribution until the desired 
#'   acceptance rate is achieved. If a scale is given, it will be used
#'   to adapt the number of particles at each iteration.
#' @param wrapper \code{bi_wrapper} (which has been run) to study
#' @param init initial number of particles (default: 1)
#' @param min minimum acceptance rate
#' @param max maximum acceptance rate
#' @param scale scale multiplier/divider for the number of particles (default: 2).
#'   If >1 this will be inverted and rounded up.
#' @param add_options list of additional options
#' @param samples number of samples to generate each iteration
#' @param max_iter maximum of iterations (default: 10)
#' @param ... parameters for bi_wrapper$run
#' @return a \code{bi_wrapper} with the desired proposal distribution
#' @export
adapt_particles <- function(wrapper, init = 1, min = 0, max = 1, scale = 2, add_options, samples, max_iter = 10, ...) {

  if (missing(add_options)) {
    add_options <- list()
  } else if (!is.list(add_options)) {
    stop("'add_options' must be given as list.")
  }

  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  nParticles <- init

  ## scale should be > 1 (it's a multiplier if acceptance rate is too
  ## small, divider if the acceptance Rate is too big)
  if (scale < 1) scale <- ceiling(1 / scale)

  model <- wrapper$model
  model$remove_block("proposal_parameter")
  model$remove_block("proposal_initial")
  
  init_file <- wrapper$output_file_name
  init_np <- bi_dim_len(init_file, "np") - 1 ## use last parameter value from output file

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
  add_options[["nparticles"]] <- nParticles
  adapt_wrapper <-
    wrapper$clone(model = model, run = TRUE, add_options = add_options, ...)
  add_options[["init-file"]] <- adapt_wrapper$output_file_name
  add_options[["init-np"]] <- samples - 1
  iter <- 1
  while ((length(accRate) == 0 | min(accRate) < min | max(accRate) > max) && iter <= max_iter && nParticles > 1) {
    if (length(accRate) == 0 | min(accRate) < min) {
      nParticles <- nParticles * scale
    } else {
      nParticles <- ceiling(nParticles / scale)
    }
    cat("Trying ", nParticles, " particles \n")
    add_options[["nparticles"]] <- nParticles
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
