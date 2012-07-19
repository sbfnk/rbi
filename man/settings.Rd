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
  This class holds all the parameters of the Parallel Adaptive Wang-Landau algorithm that are related to the bins:
  it includes the functions that take points and return the point locations with respect to the bins, parameters related to the number of bins, the split mechanism, the adaptation rate of the stochastic approximation schedule, etc.
}
\section{Objects from the Class}{
Objects should created by calls of the function \code{settings}.
Examples are provided that should help understanding this class. Essentially it is a list of parameters, most of which have a reasonable default value so you do not need to think about it too much.
}
\section{Important slots}{
  \describe{
    \item{\code{position}:}{Object of class \code{"function"}: should be a function taking points and associated log density values, and returning a "reaction coordinate", that is, a value that will be associated with bins. Typically, it can be the log density itself, or one component of a d-dimensional point. See the example below.}
    \item{\code{binrange}:}{ Object of class \code{"numeric"}: it should be a vector of size 2, holding the minimum and the maximum on the reaction coordinate scale. The bins are going to be between those two (inner bins), while a bin will go from - infinity to the minimum, and a bin will go from maximum to + infinity (outer bins).
    }
    \item{\code{ncuts}:}{ Object of class \code{"numeric"}: how many cuts will be made in the bin range specified by the previous argument. This induce the number of initial bins. Bins are automatically created by the following line: 

      \code{
      bins <- c(-Inf, seq(from = binrange[1], to = binrange[2], length.out = ncuts))}

    There are then (ncuts +1) bins. The default for ncuts is 9, resulting in 10 bins.
    }
  }
}
\section{Optional slots}{
  \describe{
    \item{\code{bins}:}{ Object of class \code{"numeric"}: you can specify the bins directly, in which case you do not need to specify \code{binrange}.
    }
    \item{\code{name}:}{ Object of class \code{"character"}: ... if you want to name the instance (default is "unspecified").
    }
    \item{\code{autosettings}:}{ Object of class \code{"logical"}: activate or not the splitting mechanism, to create new inner bins automatically. This does not create new bins outside the specified bin range, it just add new bins inside to help reaching the Flat Histogram criteria more quickly.
    }
    \item{\code{desiredfreq}:}{ Object of class \code{"numeric"}: you can specify the desired frequency of each bin. The default is 1 / nbins in each bin, where nbins is the number of bins. Note that if autosettings is enable, when a bin is split into two bins, the desired frequencies of the new bins are equal to half of the desired frequency of the former bin.
    }
    \item{\code{useLearningRate}:}{ Object of class \code{"logical"}: active or not the stochastic approximation schedule. That is, if it is not activated, then no schedule are used in the update of theta (the penalty associated to the bins). Default is TRUE.
    }
    \item{\code{useFH}:}{ Object of class \code{"logical"}: active or not the Flat Histogram checks. If it is not activated, then the stochastic approximation decreases at each step. Default is TRUE, unless useLearningRate is FALSE, in which case there is no point checking for Flat Histograms.
    }
    \item{\code{fhthreshold}:}{ Object of class \code{"numeric"}: specifies the threshold to accept Flat Histogram. The default is 0.5. Smaller values make the Flat Histogram criterion harder to reach. 
    }
    \item{\code{minSimEffort}:}{ Object of class \code{"numeric"}: specifies the minimum number of iterations after a Flat Histogram, for a new Flat Histogram criterion to be accepted. It prevents the criterion to be accepted at every iteration when using a large number of parallel chains. Default is 200.
    }
    \item{\code{learningrate}:}{ Object of class \code{"function"}: specifies the learning rate, that is, the rate at which the stochastic schedule decreases. It should be a function defined on [0, + infty[ such that it is not integrable but its square is integrable, e.g. t -> 1/t for instance. The default is t -> t^{-0.6}.
    }
    \item{\code{splitThreshold}:}{ Object of class \code{"numeric"}: specifies the threshold to split a bin into two new bins. The default is 0.1 (read 10\%), which means that a bin is split if at least 90\% of the points in that bin are on the half right (or left) side of the bin. Larger values (e.g. 25\%) result in more splits, and hence more final bins.
    }
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
\author{
  Luke Bornn <l.bornn@stat.ubc.ca>,
  Pierre Jacob <pierre.jacob@ensae.fr>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
  showClass("settings")
  getPos <- function(points, logdensity) points
  positionsettings <- settings(position = getPos,
                        name = "position",
                        binrange = c(-4, 0),
                        ncuts = 4,
                        autosettings = TRUE,
                        useLearningRate = TRUE)
}
\keyword{classes}
