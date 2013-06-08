\name{bi_libbi}
\alias{bi_libbi}
\alias{libbi}
\title{R interface to libbi}
\usage{
  bi_libbi(bi_settings, args, outputfile = "",
    stdoutputfile = "")
}
\arguments{
  \item{bi_settings}{a object created by a call to
  \code{\link{bi_settings}}}

  \item{args}{additional arguments to pass to the call to
  \code{libbi}}

  \item{outputfile}{path to the result file (which will be
  overwritten)}

  \item{stdoutputfile}{path to a file to text file to
  report the output of \code{libbi}}
}
\value{
  a list containing the absolute paths to the results
}
\description{
  \code{bi_libbi} allows to launch \code{libbi} with a
  particular set of command line arguments. This allows the
  user to perform the entire analysis without exiting R.
}

