\name{settings}
\alias{settings-class}
\alias{settings}
\alias{settings-methods}
\alias{settings-method}
\alias{settings,ANY-method}
\Rdversion{1.1}
\docType{class}
\alias{show,settings-method}

\title{Class \code{"settings"}}
\description{
}
\section{Objects from the Class}{
Objects should created by calls of the function \code{settings}.
}
\section{Slots}{
  \describe{
    \item{\code{mode}:}{Object of class \code{"character"}: first command to pass to bi, e.g. "sample".}
    \item{\code{configfile}:}{ Object of class \code{"character"}: points to a config file located in the model directory, e.g. "sample.conf".}
    \item{\code{args}:}{ Object of class \code{"character"}: additional arguments to complete those of the config file, e.g. "-T 10". Those override arguments provided in the config file.}
    \item{\code{pathModel}:}{ Object of class \code{"character"}: path to a model directory e.g. containing a .conf file and a bi file.}
    \item{\code{pathBi}:}{ Object of class \code{"character"}: path to bi binary. Default is "~/PathToBiBin/" so the easiest might be to create a symbolic link to the appropriate directory with this name, e.g. 'cd; ln -s /path/to/bi/bin/ PathToBiBin;'.}
    \item{\code{pathLibs}:}{ Object of class \code{"character"}: path to bi libraries. Default is "~/PathToBiLibs/" so the easiest might be to create a symbolic link to the appropriate directory with this name, e.g. 'cd; ln -s /path/to/bi/libs/ PathToBiLibs;'.} 
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "settings")}: provides a little summary of a settings object when called (or when \code{print} is called).}
   }
}
%\references{
%%  ~~put references to the literature/web site here~~
%}
%\author{
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link{bi}},   \code{\link{launcher}}
}
\keyword{classes}
