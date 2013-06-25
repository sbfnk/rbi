#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This is a wrapper around \code{libbi sample --target joint}, to generate
#' synthetic dataset from a model. Parameters can be passed, otherwise
#' they are generated from the prior specified in the model file.
#' @param endtime final time index, so that data is generated from time 0 to time "endtime".
#' @param modelfile path to a model .Bi file.
#' @param path_to_model path to the folder in which there is the modelfile; default to "".
#' @param outputfile path to the output file on which to write the output; default to a random name.
#' @param parameters a list of parameters to be used for the generation of synthetic dataset; 
#' by default parameters are generated from the prior;
#' @param initfile if no parameters are specified, a NetCDF init file can be given directly.
#' @noutputs number of output points to be extracted from the hidden process; default is noutputs = endtime.
#' @return path to the output file.
#' @export
#' 
bi_generate_dataset <- function(endtime, modelfile, path_to_model, 
                                outputfile, parameters, initfile, noutputs){
  if (missing(endtime)){
    stop("please specify the final time index!")
  }
  if (missing(modelfile)){
    stop("please specify a model file!")
  }
  if (missing(path_to_model)){
    path_to_model <- ""
  }
  modelfile <- tools::file_path_as_absolute(absolute_path(filename=modelfile, dirname=path_to_model))
  if (missing(outputfile)){
    outputfile <- tempfile(pattern="outputfile")
  }
  if (missing(parameters)){
    if (missing(initfile)){
      with_init <- FALSE
    } else {
      with_init <- TRUE
    }
  } else {
    initfile <- tempfile(pattern="initfile")
    bi_init_file(initfile, parameters)
    with_init <- TRUE
  }
  if (missing(noutputs)){
    #noutputs missing, default to endtime
    noutputs <- endtime
  }
  bi_object <- bi_wrapper$new(client = "sample", 
        global_options = paste("--target joint --model-file", modelfile, "--nsamples 1"),
        path_to_model = path_to_model)
  if (with_init){
    bi_object$run(add_options = paste("--end-time", endtime, "--noutputs", noutputs,
                                      "--init-file", initfile,
                                      "--verbose --nthreads 1"),
                  outputfile = outputfile)
  } else {    
    bi_object$run(add_options = paste("--end-time", endtime, "--noutputs", noutputs,
                                      "--verbose --nthreads 1"),
                  outputfile = outputfile)
  }
  return(bi_object$result$outputfile)
}

