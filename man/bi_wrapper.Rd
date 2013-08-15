\name{bi_wrapper}
\alias{bi_wrapper}
\title{Bi Wrapper}
\arguments{
  \item{client}{is either "draw", "filter", "sample"... see
  LibBi documentation.}

  \item{model_file_name}{path to a model file (typically
  ending in ".bi"); libbi will be executed from within the
  same folder.}

  \item{config}{path to a configuration file, containing
  multiple arguments}

  \item{global_options}{additional arguments to pass to the
  call to \code{libbi}, on top of the ones in the config
  file}

  \item{working_folder}{path to a folder from which to run
  \code{libbi}; default to the folder where model_file_name
  is.}

  \item{path_to_libbi}{path to \code{libbi} binary; by
  default it tries to locate \code{libbi} using the
  \code{which} Unix command, after having loaded
  "~/.bashrc" if present; if unsuccessful it tries
  "~/PathToBiBin/libbi"; if unsuccessful again it fails.}
}
\description{
  \code{bi_wrapper} allows to call \code{libbi}. Upon
  creating a new bi_wrapper object, the following arguments
  can be given. Once the instance is created, \code{libbi}
  can be run through the \code{run} method documented in
  \code{\link{bi_wrapper_run}}.
}
\examples{
bi_object <- bi_wrapper$new(client = "sample",
                            model_file_name = system.file(package="bi", "PZ.bi"),
                            global_options = "--sampler smc2")
}
\seealso{
  \code{\link{bi_wrapper_run}}
}

