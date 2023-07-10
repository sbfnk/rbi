#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This function is deprecated and has been renamed to
#' \code{\link{generate_dataset}}
#' @param ... arguments to be passed to \code{\link{sample.libbi}}, especially
#'   'model', 'end_time' and 'seed'.
#' @inheritParams run
#' @return a \code{libbi} object, the generated data set
#' @export
bi_generate_dataset <- function(..., output_every = 1) {

  warning(
    "`bi_generate_dataset` has been deprecated and will be removed from ",
    "future versions of `rbi`. Use the `generate_dataset` function instead."
  )

  options <- list(...)

  options[["target"]] <- "joint"
  options[["nsamples"]] <- 1

  if (!("noutputs" %in% names(options) && missing(output_every))) {
    options[["output_every"]] <- output_every
  }
  bi_object <- do.call(sample, options)

  return(bi_object)
}

#' @rdname generate_dataset
#' @name generate_dataset
#' @title Generate Dataset
#' @description This is a wrapper around \code{libbi sample --target joint
#'   --nsamples 1}, to generate a synthetic dataset from a model. Parameters can
#'   be passed via the 'init' option (see \code{\link{run.libbi}}, otherwise
#'   they are generated from the prior specified in the model. The end time
#'   should be specified using the "end_time" option. If this is not given, only
#'   a parameter set is sampled. Use the 'noutputs' or 'output_every' options to
#'   control the number of data points being generated. By default, output_every
#'   is set to 1.
#' @param ... arguments to be passed to \code{\link{sample.libbi}}, especially
#'   'model', 'end_time' and 'seed'.
#' @inheritParams run
#' @return a \code{libbi} object, the generated data set
#' @export
generate_dataset <- function(..., output_every = 1) {

  options <- list(...)

  options[["target"]] <- "joint"
  options[["nsamples"]] <- 1

  if (!("noutputs" %in% names(options) && missing(output_every))) {
    options[["output_every"]] <- output_every
  }
  bi_object <- do.call(sample, options)

  return(bi_object)
}
