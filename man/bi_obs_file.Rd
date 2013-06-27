\name{bi_obs_file}
\alias{bi_obs_file}
\alias{bi_obs_file,}
\alias{initfile}
\alias{obs_file,}
\title{Bi Obs File}
\usage{
  bi_obs_file(filename, variable, name = "Y")
}
\arguments{
  \item{filename}{a path to a NetCDF file to write the
  variable into (will be overwritten!)}

  \item{variable}{a \code{numeric} vector.}

  \item{name}{a string representing the name to be used in
  the NetCDF file; default to "Y".}
}
\value{
  None, but creates a NetCDF file at the specified path.
}
\description{
  This function creates an observation file. This file can
  then be passed to \code{libbi} using the
  \code{--obs-file} option.
}

