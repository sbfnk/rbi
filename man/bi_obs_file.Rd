\name{bi_obs_file}
\alias{bi_obs_file}
\title{Create Observation Files for LibBi}
\usage{
  bi_obs_file(filename, variable, name = "Y")
}
\arguments{
  \item{filename}{a path to a NetCDF file to write the
  variable into, which will be overwritten if it already
  exists.}

  \item{variable}{a \code{numeric} vector of observations.}

  \item{name}{a string representing the name to be used in
  the NetCDF file; default to "Y".}
}
\value{
  None, but creates a NetCDF file at the specified path.
}
\description{
  This function creates a NetCDF obsersation file given a
  numeric vector. This file can then be passed to
  \code{libbi} using the \code{--obs-file} option.
}
\note{
  Note that it creates a time variable with indices
  starting from 1, and not from 0.
}

