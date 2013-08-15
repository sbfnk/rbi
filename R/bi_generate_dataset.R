#' @rdname bi_generate_dataset
#' @name bi_generate_dataset
#' @title Bi Generate Dataset
#' @description
#' This is a wrapper around \code{libbi sample --target joint}, to generate
#' synthetic dataset from a model. Parameters can be passed, otherwise
#' they are generated from the prior specified in the model file.
#' @param endtime final time index, so that data is generated from time 0 to time "endtime".
#' @param model_file_name path to a model .Bi file.
#' @param output_file_name path to the output file on which to write the output; default to a random name.
#' @param parameters a list of parameters to be used for the generation of synthetic dataset; 
#' by default parameters are generated from the prior;
#' @param working_folder path to a folder from which to run \code{libbi}; default to the folder where model_file_name is.
#' @param init_file_name if no parameters are specified, a NetCDF init file can be given directly.
#' @param noutputs number of output points to be extracted from the hidden process; default is noutputs = endtime.
#' @param args additional arguments to be passed to libbi.
#' @return path to the output file.
#' @export
#' 
bi_generate_dataset <- function(endtime, model_file_name, 
                                output_file_name, parameters, 
                                init_file_name, 
                                working_folder,
                                noutputs, args){
  if (missing(endtime)){
    stop("please specify the final time index!")
  }
  if (missing(model_file_name)){
    stop("please specify a model file!")
  } else {
    model_file_name <- absolute_path(model_file_name)
  }
  if (missing(working_folder)){
    working_folder <- dirname(model_file_name)
  } else {
    working_folder <- working_folder
  }
  if (missing(output_file_name)){
    output_file_name <- tempfile(pattern="output_file_name", fileext=".nc")
  }
  if (missing(parameters)){
    if (missing(init_file_name)){
      with_init <- FALSE
    } else {
      with_init <- TRUE
    }
  } else {
    init_file_name <- tempfile(pattern="init_file_name")
    bi_init_file(init_file_name, parameters)
    with_init <- TRUE
  }
  if (missing(noutputs)){
    #noutputs missing, default to endtime
    noutputs <- endtime
  }
  if (missing(args)){
    args <- ""
  }
  bi_object <- bi_wrapper$new(client = "sample", 
                              model_file_name = model_file_name,
                              working_folder = working_folder,
                              global_options = paste("--target joint --nsamples 1", args))
  if (with_init){
    bi_object$run(add_options = paste("--end-time", endtime, "--noutputs", noutputs,
                                      "--init-file", init_file_name,
                                      "--verbose --nthreads 1"),
                  output_file_name = output_file_name)
  } else {    
    bi_object$run(add_options = paste("--end-time", endtime, "--noutputs", noutputs,
                                      "--verbose --nthreads 1"),
                  output_file_name = output_file_name)
  }
  return(bi_object$result$output_file_name)
}

