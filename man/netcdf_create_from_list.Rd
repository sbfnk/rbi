\name{netcdf_create_from_list}
\alias{netcdf_create_from_list}
\title{Create NetCDF File from R list}
\usage{
  netcdf_create_from_list(filename, variables)
}
\arguments{
  \item{filename}{a path to a NetCDF file to write the
  variable into, which will be overwritten if it already
  exists.}

  \item{variables}{a \code{list}}
}
\value{
  None, but creates a NetCDF file at the specified path.
}
\description{
  This function creates a NetCDF file given a list.
}
\details{
  The list must follow the following rules. Each element of
  the list must itself be a list with two keys. The first
  key must be named "values" and contains a numeric vector.
  The second key must be named "dimension" and contains a
  string giving the dimension name. The name of the element
  itself is used to create the corresponding variable in
  the NetCDF file.
}
\note{
  Two elements of the given list can possibly have the same
  dimension name.
}
\examples{
filename <- tempfile(pattern="dummy", fileext=".nc")
a <- list(values = 1:3, dimension = "dim_a")
b <- list(values = 1:5, dimension = "dim_b")
c <- list(values = 5:9, dimension = "dim_b")
variables <- list(a=a, b=b, c=c)
netcdf_create_from_list(filename, variables)
bi_file_ncdump(filename)
}

