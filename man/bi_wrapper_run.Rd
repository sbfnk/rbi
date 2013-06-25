\name{bi_wrapper_run}
\alias{bi_run}
\alias{bi_wrapper_run}
\alias{libbi}
\title{Using the Bi Wrapper to Launch LibBi}
\arguments{
  \item{add_options}{additional arguments to pass to the
  call to \code{libbi}}

  \item{outputfile}{path to the result file (which will be
  overwritten)}

  \item{stdoutputfile}{path to a file to text file to
  report the output of \code{libbi}}
}
\value{
  a list containing the absolute paths to the results; it
  is stored in the \code{result} field of the instance of
  \code{\link{bi_wrapper}}.
}
\description{
  The method \code{run} of an instance of
  \code{\link{bi_wrapper}} allows to launch \code{libbi}
  with a particular set of command line arguments.
}
\examples{
# bi_object <- bi_wrapper$new(client = "sample", global_options = "--sampler smc2",
#                             config = "posterior.conf",
#                             path_to_model = "~/workspace/pz")
# bi_object$run(add_options=" --verbose --nthreads 1",
#               outputfile = "results/launchPZ_SMC2.nc")
# bi_file_summary(bi_object$result$outputfile)
}

