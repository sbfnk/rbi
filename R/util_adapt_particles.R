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
#' @param add_options list of additional options
#' @param samples number of samples to generate each iteration
#' @param max_iter maximum of iterations (default: 10)
#' @param max_particles maximum number of particles
#' @param ... parameters for bi_wrapper$run
#' @return a \code{bi_wrapper} with the desired proposal distribution
#' @export
adapt_particles <- function(wrapper, init = 1, min = 0, max = 1, add_options, samples, max_iter = 10, max_particles = 32768, ...) {

  if (missing(add_options)) {
    add_options <- list()
  } else if (!is.list(add_options)) {
    stop("'add_options' must be given as list.")
  }

  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  nParticles <- init

  model <- bi_model(lines = wrapper$model$get_lines())
  model$remove_block("proposal_parameter")
  model$remove_block("proposal_initial")
  model$remove_block("parameter")
  
  init_file <- wrapper$output_file_name
  init_np <- bi_dim_len(init_file, "np") - 1 ## use last parameter value from output file

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
  add_options[["nparticles"]] <- NULL
  iter <- 1
  accRate <- acceptance_rate(adapt_wrapper)
  
  while ((accRate < min | accRate > max) && iter <= max_iter &&
    (!(accRate > max && nParticles > 1)) && nParticles < max_particles) {
    nParticles <-
      min(max_particles,
          ifelse(accRate > 0,
                 2 ** (round(log(min/accRate, 2))) * nParticles,
                 2 * nParticles))
    cat(paste0("Acceptance rate ", accRate,
               ", trying ", nParticles, " particles \n"))
    adapt_wrapper$global_options[["nparticles"]] <- nParticles
    add_options[["init-file"]] <- adapt_wrapper$output_file_name
    adapt_wrapper <-
      adapt_wrapper$clone(model = model, run = TRUE, add_options = add_options, ...)
    accRate <- acceptance_rate(adapt_wrapper)
    iter <- iter + 1
  }
  cat("Acceptance rate:", min(accRate), "\n")

  wrapper$global_options[["nparticles"]] <- nParticles
  
  if (iter > max_iter) {
    warning("Maximum of iterations reached")
  }

  return(wrapper)
}
