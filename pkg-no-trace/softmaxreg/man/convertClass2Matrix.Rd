\name{convertClass2Matrix}
\alias{convertClass2Matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert A Vector of Factors to Matrix
}
\description{
Convert A Vector of Factors to Matrix
}
\usage{
convertClass2Matrix(target)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
Vector of factor representing each class.
}
}
\details{
This Function can be used to convert factor to matrix yMat, e.g. For type 'raw' softmaxReg function input yMat, softmax regression.
}
\value{
Matrix with dimensions number of observation * number of class factors
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{softmaxReg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## This Function can be used to convert factor to matrix yMat.
## e.g. For type 'raw' softmaxReg function input yMat, softmax regression.
y = as.factor(c(rep(1,50),rep(2,50),rep(3,50)))
yMat = convertClass2Matrix(y)
nObs = dim(yMat)[1]
K = dim(yMat)[2]
nObs
K
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
