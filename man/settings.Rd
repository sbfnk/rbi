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
    \item{\code{Mode}:}{Object of class \code{"character"}:}
    \item{\code{ConfigFile}:}{ Object of class \code{"character"}:}
    \item{\code{Args}:}{ Object of class \code{"character"}:}
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
