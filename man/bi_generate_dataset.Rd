\name{bi_generate_dataset}
\alias{bi_generate_dataset}
\title{Bi Generate Dataset}
\usage{
  bi_generate_dataset(endtime, modelfile, path_to_model,
    outputfile, parameters, initfile, noutputs, args)
}
\arguments{
  \item{endtime}{final time index, so that data is
  generated from time 0 to time "endtime".}

  \item{modelfile}{path to a model .Bi file.}

  \item{path_to_model}{path to the folder in which there is
  the modelfile; default to "".}

  \item{outputfile}{path to the output file on which to
  write the output; default to a random name.}

  \item{parameters}{a list of parameters to be used for the
  generation of synthetic dataset; by default parameters
  are generated from the prior;}

  \item{initfile}{if no parameters are specified, a NetCDF
  init file can be given directly.}
}
\value{
  path to the output file.
}
\description{
  This is a wrapper around \code{libbi sample --target
  joint}, to generate synthetic dataset from a model.
  Parameters can be passed, otherwise they are generated
  from the prior specified in the model file.
}

