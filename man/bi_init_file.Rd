\name{bi_init_file}
\alias{bi_init_file}
\alias{bi_init_file,}
\alias{initfile}
\alias{init_file,}
\title{Bi Init File}
\usage{
  bi_init_file(filename, variables)
}
\arguments{
  \item{filename}{a path to a NetCDF file to write the
  variables into (will be overwritten!)}

  \item{variables}{a \code{list} object, which names should
  be the variable names and values should be vectors of
  equal length (or simply one value per key)}
}
\value{
  None, but creates a NetCDF file at the specified path.  
}
\description{
  This function creates an init file to specify parameter
  values and initial conditions. This file can then be
  passed to \code{libbi} using the \code{--init-file}
  option.
}

