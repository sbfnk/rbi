% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locate_libbi.r
\name{locate_libbi}
\alias{locate_libbi}
\title{Find the libbi executable}
\usage{
locate_libbi(path_to_libbi)
}
\arguments{
\item{path_to_libbi}{path to libbi, as either the path where the libbi
executable resides, or the full path to the executable}
}
\value{
full path to the libbi executable as character string; if it is not
  found, an error is thrown
}
\description{
This tries to find the libbi executable; if this does not find libbi but it
is installed, the location can be either passed to this function, or set
globally via `options(path_to_libbi="/insert/full/path/here").
}
\author{
Sebastian Funk
}
\keyword{internal}
