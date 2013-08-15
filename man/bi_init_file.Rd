\name{bi_init_file}
\alias{bi_init_file}
\title{Create Init Files for LibBi}
\usage{
  bi_init_file(filename, variables)
}
\arguments{
  \item{filename}{a path to a NetCDF file to write the
  variables into, which will be overwritten if it already
  exists.}

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

