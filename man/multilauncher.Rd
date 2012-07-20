\name{multilauncher}
\alias{multilauncher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
multilauncher
}
\description{
Launch bi using the specified settings and additional arguments, a specified number of times,
using a specified seed for each run, and storing results in a file with specified name.
}
\usage{
multilauncher(settings, args = "", nruns = 1, seeds = 1:nruns, 
  filenamefunction = function(i) return(paste("results/results", i, ".nc", sep = "")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{settings}{
  Object of class \code{bi::settings}: (see \link{settings}).
}
  \item{args}{
  Additonal arguments that you might want to add; those override what is specified in the \code{settings} object.
}
  \item{nruns}{
  Integer specifying the number of runs.
}
  \item{seeds}{
  Vector of size \code{nruns} containing integers used for seeding the RNG of each bi run.
}
  \item{filenamefunction}{
  Function taking an integer and returning a string representing file name.
}
}


\seealso{
  \code{\link{settings}},  \code{\link{launcher}}
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
