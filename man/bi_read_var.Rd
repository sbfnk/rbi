\name{bi_read_var}
\alias{bi_read_var}
\title{Bi Read Variable}
\usage{
  bi_read_var(resultfile, name, coord, ps, ts)
}
\arguments{
  \item{resultfile}{either a path to a NetCDF file, or a
  NetCDF connection created using open.ncdf(filename)}

  \item{name}{name of the variable to read (use
  \code{\link{bi_file_summary}} to learn about the variable
  names of a specific file)}

  \item{coord}{dimension indices (not implemented yet)}

  \item{ts}{time indices (not implemented yet)}
}
\description{
  This function reads a variable from a NetCDF file. The
  file can be specified as a string to the filepath, in
  which case a NetCDF connection is opened, or directly as
  a NetCDF connection.
}

