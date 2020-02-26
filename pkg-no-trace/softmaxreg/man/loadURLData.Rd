\name{loadURLData}
\alias{loadURLData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download and Unzip Web Datasets
}
\description{
Download web datasets from URL to local path and unzip the data.
}
\usage{
loadURLData(URL, folder, unzip = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{URL}{
String of the url of the web dataset.
}
  \item{folder}{
String of the path of the folder you want to put the downloaded files. Folder path will be set as the working directory.
}
  \item{unzip}{
Boolean variable. If true, the ".zip" files will be unzipped.
}
}
\seealso{
\code{\link{document}}
\code{\link{wordEmbed}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# Download UCI Archived Dataset from URL:
# http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip
# Reuter 50 DataSet
URL = "http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip"
folder = getwd()
loadURLData(URL, folder, unzip = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
