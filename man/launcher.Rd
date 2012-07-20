\name{launcher}
\alias{launcher}
\title{
  launcher
}
\description{
  Launch bi using the specified settings and additional arguments.
}
\usage{
  launcher(settings, args)
}
\arguments{
  \item{settings}{ Object of class \code{bi::settings}: (see \link{settings}).
  }
  \item{args}{ Additonal arguments that you might want to add; those override what is specified in the \code{settings} object. Useful for instance if you want to specify different seeds at launch time, but without modifying the settings object. See also \link{multilauncher} for launching bi multiple times. 
  }
}
\details{
}
\value{
}
\seealso{
  \code{\link{bi}},   \code{\link{settings}}
}
