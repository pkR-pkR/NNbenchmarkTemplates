\name{trainModel}
\alias{trainModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Train softmax regression and classification model
}
\description{
This function implements a feedforward neural networks with multiple hidden layers and a softmax final layer. For classification, set type as "class" and y as factor vector; for regression, set type as "raw" and y as matrix with dimension nObs * K. K denotes the group number.
}
\usage{
trainModel(x, y, hidden, funName, maxit, rang, type, algorithm, rate,
    L2, penalty, threshold, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix or data frame of x input values.
}
  \item{y}{
vector of target values for 'class' type classfication and matrix or data frame of target values for 'raw' type regression.
}
  \item{hidden}{
vector of integers specifying the number of hidden nodes in each layer.
}
  \item{funName}{
activation function name of neuron, e.g. 'sigmoid', 'tanh', 'relu' etc. In default, it is set to 'sigmoid'.
}
  \item{maxit}{
maximum number of iterations.Default 3000.
}
  \item{rang}{
parameter for the range of initial random weights. Default 0.1 [-rang, rang].
}
  \item{type}{
parameter indicating the type of softmax task: "class" denotes the softmax classfication model and the fitted values are factors; "raw" denotes softmax regression model and the fitted values are
the probability or percentage of each group. Default "class".
}
  \item{algorithm}{
parameter indicating which gradient descenting learning algorithm to use, including "sgd", "adagrad",  "rmsprop", "adadelta", etc. Default "adagrad".
}
  \item{rate}{
parameter of learning rate. Default 0.05.
}
  \item{L2}{
Boolean variable indicating whether L2 regularization term is added to the loss function and gradient to prevent overfitting. Default FALSE.
}
  \item{penalty}{
Parameter for the penalty cost of the L2 regularization term if L2 is TRUE. Default 1e-4.
}
  \item{threshold}{
Parameter for the threshold of iteration convergence: loss value less than threshold. Default 1e-4.
}
  \item{batch}{
Parameter for mini-batch size. Default 50.
}
}
\value{
object of class "softmax"
  \item{weights}{
Optimal weights parameters found by softmax model, including list of W and B for all layers.
}
  \item{data}{
Input Training Data.
}
  \item{K}{
Number of K groups fitted by softmax model.
}
  \item{loss}{
Numeric vector of the loss function values over iterations.
}
  \item{fitted.values}{
Matrix of the fitted values yFitMat for the training data. Dimensions: number of observations by K;
}
  \item{iteration}{
Number of iteration reached before stop.
}
  \item{convergence}{
Boolean variable for whether softmax model reached convergence.
}
}
\seealso{
\code{\link{softmaxReg}}
}
\examples{
\dontrun{
library(softmaxreg)
data(iris)
x = iris[,1:4]
y = iris$Species
sofmax_model = trainModel(x, y, hidden = c(5), funName = 'sigmoid', maxit = 3000,
    rang = 0.1, type = "class", algorithm = "adagrad", rate = 0.05, threshold = 1e-3)
summary(sofmax_model)
yFitMat = sofmax_model$fitted.values
yFit = c()
for (i in 1:length(y)) {
	yFit = c(yFit, which(yFitMat[i,]==max(yFitMat[i,])))
}
table(y, yFit)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
