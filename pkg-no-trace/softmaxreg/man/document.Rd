\name{document}
\alias{document}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy Implementation to Read Multiple Documents within the Folder
}
\description{
Easy implementation to read multiple documents within the folder with extension pattern.
}
\usage{
document(path, name = NULL, pattern = "txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Character Vectors representing the folders' path. One element of string denotes reading the document from one folder and a vector of characters denotes reading the documents from multiple folders simultaneously.
}
  \item{name}{
Character representing the name of the specific file to read. Default NULL. If NULL, function will read all the text files in that folder.
}
  \item{pattern}{
Character for the file extensions of the text files, like "txt", "csv", etc. Default "txt".
}
}
\value{
Vectors of characters, each element in the vector contains the text of one file.
}
\seealso{
\code{\link{loadURLData}}
\code{\link{wordEmbed}}
}
\examples{
\dontrun{
path = "your_local_path"
docs = document(path, pattern = 'txt')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
