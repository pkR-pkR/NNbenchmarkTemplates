\name{wordEmbed}
\alias{wordEmbed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Embed Words to Vectors Using Pre-trained Word2vec Dictionary
}
\description{
Embed words in string to vectors using the pre-trained word2vec dictionary.
User can also replace the word2vec dataframe with customized data.
}
\usage{
wordEmbed(object, dictionary, meanVec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Vectors of text representing documents.
}
  \item{dictionary}{
Dataframe of pre-trained word2vec dataset. The First column is the word and the following columns are numeric vectors from word2vec models. The default dataset with the package is a pre-trained 20 dimension word2vec dataset.
}
  \item{meanVec}{
Boolean variable. If meanVec is TRUE, a matrix is returned with each row representing the mean of numeric vectors of all the words in a document. If FALSE, a list of matrix is returned in which each document is represented by a matrix.
}
}
\value{
wordEmbed returns a matrix if meanVec is TRUE and a list of matrix if meanVec is FALSE.
}
\seealso{
\code{\link{document}}
\code{\link{word2vec}}
}
\examples{
data(word2vec) # load default 20 dimensions word2vec dataset
doc = "This is an example line of document"
docVectors = wordEmbed(doc, word2vec, meanVec = TRUE)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
