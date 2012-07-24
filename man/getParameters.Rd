\name{getParameters}
\alias{getParameters}
\title{
  getParameters
}
\description{
  Retrieve multiple parameters from files.
}
\usage{
  getParameters(filenames, variablenames, logweightname, verbose = FALSE)
}
\arguments{
  \item{filenames}{
    Input files. Either one or a vector of strings.
  }
  \item{variablenam}{
    Name of parameter from input file to retrieve.
  }
  \item{timeindex}{
    (optional)se
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
    