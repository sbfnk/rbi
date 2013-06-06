\name{bi_file_summary}
\alias{bi_file_summary}
\title{Bi File Summary}
\usage{
  bi_file_summary(filename)
}
\arguments{
  \item{filename}{path to a NetCDF file}
}
\value{
  None
}
\description{
  This function prints a little summary of the content of a
  NetCDF file, as well as its creation time. You can then
  retrieve variables of interest using
  \code{\link{bi_read_var}}.
}

