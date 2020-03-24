\name{softmax-class}
\Rdversion{1.1}
\docType{class}
\alias{softmax-class}
\alias{$,softmax-method}

\title{Class \code{"softmax"}}
\description{
"softmax" class returned by softmaxReg function for softmax regression
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("softmax", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{weights}:}{Object of class \code{"list"}}
    \item{\code{data}:}{Object of class \code{"list"}}
    \item{\code{K}:}{Object of class \code{"numeric"}}
    \item{\code{iteration}:}{Object of class \code{"numeric"}}
    \item{\code{loss}:}{Object of class \code{"numeric"}}
    \item{\code{fitted.values}:}{Object of class \code{"matrix"}}
    \item{\code{convergence}:}{Object of class \code{"logical"}}
    \item{\code{type}:}{Object of class \code{"character"}}
    \item{\code{funName}:}{Object of class \code{"character"}}
  }
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "softmax")}: ... }
	 }
}
\examples{
showClass("softmax")
}
\keyword{classes}
