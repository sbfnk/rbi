#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This is a wrapper around \code{libbi sample --target joint --nsamples 1}, to generate a
#' synthetic dataset from a model. Parameters can be passed via the 'init' option
#' (see \code{\link{run}}, otherwise they are generated from the prior specified
#' in the model. The end time should be specified using the "end_time" option. If this is not given,
#' only a parameter set is sampled. 
#' @param ... arguments to be passed to \code{\link{libbi}} and \code{\link{sample}}, especially 'model' and 'end_time'
#' @return generated data set
#' @export
bi_generate_dataset <- function(...){
  dot_options <- list(...)
  if ("options" %in% dot_options) {
    options <- dot_options[["options"]]
  } else {
    options <- list()
  }

  if (grepl("^end[-_]time$", names(dot_options)) &&
            !("noutputs" %in% names(dot_options))) {
    dot_options[["noutputs"]] <- dot_options[["end_time"]]
  }

  options[["target"]] <- "joint"
  options[["nsamples"]] <- 1

  bi_object <- do.call(libbi, c(options, dot_options))
  bi_object <- sample(bi_object)

  return(bi_object)
}

