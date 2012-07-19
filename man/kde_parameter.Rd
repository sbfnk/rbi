\name{kde_parameter}
\alias{kde_parameter}
\title{
  kde_parameter
}
\description{
  Plot kernel density estimate of parameters.
}
\usage{
  kde_parameter(netCDFfiles, variablename, timeindex, adjust, logweightname, verbose = FALSE)
}
\arguments{
  \item{netCDFfiles}{
	Input files. Either one or a vector of strings.
	}
  \item{variable}{
	Name of parameter from input file to plot.
	}
  \item{timeindex}{
  (optional)
  In case the netCDF file contains the parameter values at different times, 
  this is used to specify one of the times. Default is the latest time found in the file.
	}
  \item{adjust}{
  (optional)
  Kernel width adjustement. Default to 1. See ?ggplot2::stat_density.
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
  The function calls ggplot2 and the resulting plot should appear when the function is called.
}
\value{
	The function returns a ggplot2 object.
}

\seealso{
\code{\link{bi}}
}
