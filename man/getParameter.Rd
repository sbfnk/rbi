\name{getParameter}
\alias{getParameter}
\title{
  getParameter
}
\description{
  Retrieve parameters from files.
}
\usage{
  getParameter(files, variable, timeindex, logweightname, verbose = FALSE)
}
\arguments{
  \item{files}{
	Input files. Either one or a vector of strings.
	}
  \item{variable}{
	Name of parameter from input file to retrieve.
	}
  \item{timeindex}{
  (optional)
  In case the netCDF file contains the parameter values at different times, 
  this is used to specify one of the times. Default is to retrieve all the times.
	}
  \item{logweightname}{
  (optional)
  Name of the variable containing the log weights of the sample.
  Default is what bi's default.
	}
  \item{verbose}{
  (optional)
  Should some debugging information be output in the command line?
	}
}
\details{
  The function returns a data frame containing the parameters and their associated weights.
}
\value{
	A pretty explicit data frame.
}

\seealso{
\code{\link{bi}}
}
