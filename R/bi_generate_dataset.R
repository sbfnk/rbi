#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This is a wrapper around \code{libbi sample --target joint}, to generate
#' synthetic dataset from a model. Parameters can be passed via the 'init' option
#' (see \code{\link{libbi_run}}, otherwise they are generated from the prior specified
#' in the model.
#' @param endtime final time index, so that data is generated from time 0 to time "endtime".
#' @param noutputs number of output points to be extracted from the hidden process; default is noutputs = endtime.
#' @param ... arguments to be passed to \code{\link{libbi}} (with run = TRUE)
#' @return generated data set
#' @export
bi_generate_dataset <- function(endtime, noutputs, ...){
  full_config <- !is.null(list(...)[['config']])

  if (missing(endtime) & full_config==F){
    stop("please specify the final time index!")
  }

  if (missing(noutputs) & full_config==F) {
    #noutputs missing, default to endtime
    noutputs <- endtime
  }

  global_options <- list()

  if (full_config == F){
    global_options[["end-time"]] <- endtime
    global_options[["noutputs"]] <- endtime
    global_options[["target"]] <- "joint"
    global_options[["nsamples"]] <- 1
  } else if(is.null(list(...)[['working_folder']])){
    stop("Cannot use a config file without specifying working_folder.")
  }

  bi_object <- libbi$new(client = "sample", global_options = global_options,
                         run = TRUE, ...)
  return(bi_object)
}

