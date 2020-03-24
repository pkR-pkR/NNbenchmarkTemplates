\name{word2vec}
\alias{word2vec}
\docType{data}
\title{
Pre-trained Word2vec Dataset from Corpus
}
\description{
This dataset is a small pre-trained word2vec dataset with 20 dimensions and 5296 words.
}
\usage{data("word2vec")}
\format{
  A data frame with 12853 observations on the following 21 variables.
  \describe{
    \item{\code{word}}{character}
    \item{\code{col1}}{numeric}
    \item{\code{col2}}{numeric}
    \item{\code{col3}}{numeric}
    \item{\code{col4}}{numeric}
    \item{\code{col5}}{numeric}
    \item{\code{col6}}{numeric}
    \item{\code{col7}}{numeric}
    \item{\code{col8}}{numeric}
    \item{\code{col9}}{numeric}
    \item{\code{col10}}{numeric}
    \item{\code{col11}}{numeric}
    \item{\code{col12}}{numeric}
    \item{\code{col13}}{numeric}
    \item{\code{col14}}{numeric}
    \item{\code{col15}}{numeric}
    \item{\code{col16}}{numeric}
    \item{\code{col17}}{numeric}
    \item{\code{col18}}{numeric}
    \item{\code{col19}}{numeric}
    \item{\code{col20}}{numeric}
  }
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Reuter_50_50 dataset,UCI Machine Learning Repository[https://archive.ics.uci.edu/ml/datasets/Reuter_50_50]
}
\references{
%%  ~~ possibly secondary sources and usages ~~
word2vec model are trained based on below text corpus:

Reuter_50_50 dataset,UCI Machine Learning Repository[https://archive.ics.uci.edu/ml/datasets/Reuter_50_50],
Author: Zhi Liu, National Engineering Research Center For E-Learning Technology, Hubei Wuhan, China
}
\examples{
data(word2vec)
}
\keyword{datasets}
