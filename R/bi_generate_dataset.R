#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This is a wrapper around \code{libbi sample --target joint --nsamples 1}, to generate a
#' synthetic dataset from a model. Parameters can be passed via the 'init' option
#' (see \code{\link{libbi_run}}, otherwise they are generated from the prior specified
#' in the model. The end time should be specified using the "end_time" option. If this is not given,
#' only a parameter set is sampled. 
#' @param ... arguments to be passed to \code{\link{libbi}} (with run = TRUE), especially 'end_time'
#' @return generated data set
#' @export
bi_generate_dataset <- function(...){
  ## if (missing(endtime)){
  ##   stop("please specify the final time index!")
  ## }

  dot_options <- list(...)
  if ("options" %in% dot_options) {
    options <- dot_options[["options"]]
  } else {
    options <- list()
  }

  if ("endtime" %in% names(dot_options)) {
    warning("'endtime' is deprecated,  use 'end_time' instead")
    names(dot_options)[which(names(dot_options) == "endtime")] <- "end_time"
  }

  options[["target"]] <- "joint"
  options[["nsamples"]] <- 1

  libbi_new_options <-
    c(list(client = "sample", options = options, run = TRUE),
      dot_options)

  bi_object <- do.call(libbi$new, libbi_new_options)

  return(bi_object)
}

