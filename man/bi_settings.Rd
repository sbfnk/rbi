\name{bi_settings}
\alias{bi_settings}
\title{Bi Settings}
\arguments{
  \item{client}{is either "draw", "filter", "sample"... see
  LibBi documentation.}

  \item{config}{path to a configuration file, containing
  multiple arguments}

  \item{args}{additional arguments to pass to the call to
  \code{libbi}, on top of the ones in the config file}

  \item{path_to_libbi}{path to the libbi binary; default to
  "~/PathToBiBin/libbi", so the easiest might be to create
  a symbolic link}

  \item{path_to_model}{path to the model folder, from which
  libbi binary will be executed}
}
\description{
  \code{bi_settings} is a container for libbi arguments.
  Once an object is created, it is meant to be given to
  \code{\link{bi}}.
}

