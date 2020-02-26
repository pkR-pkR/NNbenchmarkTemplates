\name{softmaxReg}
\alias{softmaxReg}
\alias{softmaxReg.default}
\alias{softmaxReg.formula}
\alias{predict.softmax}
\alias{summary.softmax}
\title{
Fit Multi-Layer Softmax Regression or Classification Model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Fit softmax regression or classification model with multiple hidden layers neural networks and final softmax layer.
}
\usage{
softmaxReg(x, ...)

\method{softmaxReg}{default}(x, y, hidden = c(), funName = 'sigmoid', maxit = 3000,
rang = 0.1, type = "class", algorithm = "rmsprop", rate = 0.05,
L2 = FALSE, penalty = 1e-4, threshold = 1e-4, batch = 50,...)

\method{softmaxReg}{formula}(formula, data, hidden = c(), funName = 'sigmoid', maxit = 3000,
rang = 0.1,type = "class", algorithm = "rmsprop", rate = 0.05,
L2 = FALSE, penalty = 1e-4, threshold = 1e-4, batch = 50,...)

\method{predict}{softmax}(object, newdata, ...)

\method{summary}{softmax}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
Formula of form y ~ x1 + x2 + ... for 'class' type classification; And (y1 + y2+ ...+ yk) ~ x1 + x2 + ... for 'raw' type regression.
}
  \item{x}{
Matrix or data frame of x input values.
}
  \item{y}{
Vector of target values y for 'class' type classfication and matrix or data frame of target values (y1,y2,...yk) for 'raw' type regression.
}
  \item{data}{
Data frame containing the variables in formula.
}
  \item{hidden}{
Numeric vector of integers specifying the number of hidden nodes in each layer, e.g. hidden = c(8,5,...). Default NULL.
}
  \item{funName}{
Name of neural network activation function, including 'sigmoid', 'tanh', 'relu'. Default 'sigmoid'.
}
  \item{maxit}{
Integer for maximum number of iterations. Default 3000.
}
  \item{rang}{
Parameter for the range of initial random weights [-rang, rang]. Default 0.1.
}
  \item{type}{
Parameter indicating the type of softmax task: 'class' denotes the softmax classfication model and the fitted values are factors; 'raw' denotes softmax regression model and the fitted values are raw probability or percentage data of each group. Default 'class'.
}
  \item{algorithm}{
Parameter indicating which gradient descenting learning algorithm to use, including 'sgd', 'adagrad',  'rmsprop', 'adadelta', 'momentum', 'nag'(Nesterov Momentum), etc. Default 'rmsprop'.
}
  \item{rate}{
Parameter for the initial learning rate. Default 0.05.
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
\item{object}{ An object of class \code{"softmax"}, the fitted model of softmaxReg function.}
\item{newdata}{Matrix or dataframe of new Data for prediction.}
\item{\dots}{Other arguments}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function can be used to train typical n-class classification models. Also, it can be used to fit 'raw' data regression, e.g. the percentage/probability data of each group in the Multinomial Logit/Probit model, as well.
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
\references{
%% ~put references to the literature/web site here ~
MNIST Dataset HandWritten Digit Recognition: http://yann.lecun.com/exdb/mnist/

MNIST Data Reading method reuse R code from: brendan o'connor - https://gist.github.com/brendano/39760

Reuter 50 DataSet: UCI Archived Dataset:
http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip

}
\author{
Xichen Ding
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wordEmbed}}
\code{\link{document}}
\code{\link{loadURLData}}
}
\examples{
\dontrun{
#### Example 1, Softmax classification with hidden layer and no regularization term

library(softmaxreg)
data(iris)
x = iris[,1:4]
y = iris$Species
# Training with hidden layer set 5 units
softmax_model = softmaxReg(x, y, hidden = c(5), maxit = 100, type = "class",
  algorithm = "adagrad", rate = 0.05, batch = 20)
summary(softmax_model)
yFitMat = softmax_model$fitted.values
yFit = c()
for (i in 1:length(y)) {
	yFit = c(yFit, which(yFitMat[i,]==max(yFitMat[i,])))
}
table(y, yFit)
# Caculate AIC and BIC information criterion
aic = AIC(softmax_model)
bic = BIC(softmax_model)
cat("AIC",aic,'\n')
cat("BIC",bic,'\n')

# Make new Prediction
newdata = iris[1:100,1:4]
yPred = predict(softmax_model, newdata)

#### Example 2, Softmax classification with formula and dataframe input

f = formula(Species~.) # formula with succinct expression
softmax_model_fm = softmaxReg(f, data = iris, hidden = c(5), maxit = 100, type = "class",
  algorithm = "adagrad", rate = 0.05, batch = 20)
summary(softmax_model_fm)

#### Example 3: Softmax classfication with L2 regularization

softmax_model_L2 = softmaxReg(x, y, hidden = c(5), maxit = 100, type = "class",
  algorithm = "adagrad", L2 = TRUE, penalty = 1e-4, batch = 20)
summary(softmax_model_L2)

# Compare Two Model Loss
# Note L2 loss value include the ||W||^2 term, larger than loss of previous model
loss1 = softmax_model$loss
loss2 = softmax_model_L2$loss
plot(c(1:length(loss1)), loss1, xlab = "Iteration", ylab = "Loss Function Value",
  type = "l", col = "black")
lines(c(1:length(loss2)), loss2, col = "red")
legend("topright", c("Loss 1: No Regularization", "Loss 2: L2 Regularization"),
  col = c("black", "red"),pch = 1)


#### Example 4: Compare different learning algorithms 'adagrad','sgd',
# 'rmsprop', 'momentum', 'nag' (Nesterov Momentum)

library(softmaxreg)
data(iris)
x = iris[,1:4]
y = iris$Species
model1 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 100, rang = 0.1,
  type = "class", algorithm = "sgd", rate = 0.1, batch = 150)
loss1 = model1$loss

model2 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 100, rang = 0.1,
  type = "class", algorithm = "adagrad", rate = 0.1, batch = 150)
loss2 = model2$loss

model3 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 100, rang = 0.1,
  type = "class", algorithm = "rmsprop", rate = 0.1, batch = 150)
loss3 = model3$loss

model4 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 100, rang = 0.1,
  type = "class", algorithm = "momentum", rate = 0.1, batch = 150)
loss4 = model4$loss

model5 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 100, rang = 0.1,
  type = "class", algorithm = "nag", rate = 0.1, batch = 150)
loss5 = model5$loss

# plot the loss convergence
iteration = c(1:length(loss1))
plot(iteration, loss1, xlab = "iteration", ylab = "loss", ylim = c(0,
   max(loss1,loss2,loss3,loss4,loss5) + 0.01), type = "p", col = "black", cex = 0.7)
title("Convergence Comparision Between Learning Algorithms")
points(iteration, loss2, col = "red", pch = 2, cex = 0.7)
points(iteration, loss3, col = "blue", pch = 3, cex = 0.7)
points(iteration, loss4, col = "green", pch = 4, cex = 0.7)
points(iteration, loss5, col = "magenta", pch = 5, cex = 0.7)

legend("topright", c("SGD", "Adagrad", "RMSprop", "Momentum", "NAG"),
    col = c("black", "red", "blue", "green", "magenta"),pch = c(1,2,3,4,5))

## Comments: From this experiments we can see that momemtum learning algorithm
## generally converge faster than the standard sgd and its variations


#### Example 5: Multiple class classification: Read Online Dataset and make document classification

library(softmaxreg)
data(word2vec) # default 20 dimension word2vec dataset
#### Reuter 50 DataSet UCI Archived Dataset from
## URL: "http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip"
URL = "http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip"
folder = getwd()
loadURLData(URL, folder, unzip = TRUE)

##Training Data
subFoler = c('AaronPressman', 'AlanCrosby', 'AlexanderSmith', 'BenjaminKangLim', 'BernardHickey')
docTrain = document(path = paste(folder, "\\C50train\\",subFoler, sep = ""), pattern = 'txt')
xTrain = wordEmbed(docTrain, dictionary = word2vec)
yTrain = c(rep(1,50), rep(2,50), rep(3,50), rep(4,50), rep(5,50))
# Assign labels to 5 different authors

##Testing Data
docTest = document(path = paste(folder, "\\C50test\\",subFoler, sep = ""), pattern = 'txt')
xTest = wordEmbed(docTest, dictionary = word2vec)
yTest = c(rep(1,50), rep(2,50), rep(3,50), rep(4,50), rep(5,50))
samp = sample(250, 50)
xTest = xTest[samp,]
yTest = yTest[samp]

## Train Softmax Classification Model, 20-10-5
softmax_model = softmaxReg(xTrain, yTrain, hidden = c(10), maxit = 500, type = "class",
    algorithm = "nag", rate = 0.1, L2 = TRUE)
summary(softmax_model)
yFit = predict(softmax_model, newdata = xTrain)
table(yTrain, yFit)

## Testing
yPred = predict(softmax_model, newdata = xTest)
table(yTest, yPred)

#### Comments: Increase the word2vec dimensions to 50 or even 100 will help increase
#### the capacity of the model and prediction precision

#### Example 6: 'MNIST' dataset HandWritten Digit Recognition
## Download MNIST Dataset from below URL and Gunzip them
## http://yann.lecun.com/exdb/mnist/
## MNIST Data Reading method reuse R code from:
## brendan o'connor - https://gist.github.com/brendano/39760

library(softmaxreg)
# Replace with your local path
path = "D:\\DeepLearning\\MNIST\\"

## 10-class classification, Digit 0-9
x = load_image_file(paste(path,'train-images-idx3-ubyte', sep=""))
y = load_label_file(paste(path,'train-labels-idx1-ubyte', sep=""))
xTest = load_image_file(paste(path,'t10k-images-idx3-ubyte',sep=""))
yTest = load_label_file(paste(path,'t10k-labels-idx1-ubyte', sep=""))

## Normalize Input Data
x = x/255
xTest = xTest/255

## Compare Convergence Rate of MNIST dataset
model1 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 50, rang = 0.1,
    type = "class", algorithm = "sgd", rate = 0.01, batch = 100)
loss1 = model1$loss
model2 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 50, rang = 0.1,
    type = "class", algorithm = "adagrad", rate = 0.01, batch = 100)
loss2 = model2$loss
model3 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 50, rang = 0.1,
    type = "class", algorithm = "rmsprop", rate = 0.01, batch = 100)
loss3 = model3$loss
model4 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 50, rang = 0.1,
    type = "class", algorithm = "momentum", rate = 0.01, batch = 100)
loss4 = model4$loss
model5 = softmaxReg(x, y, hidden = c(), funName = 'sigmoid', maxit = 50, rang = 0.1,
    type = "class", algorithm = "nag", rate = 0.01, batch = 100)
loss5 = model5$loss

# plot the loss convergence
iteration = c(1:length(loss1))
myplot = plot(iteration, loss1, xlab = "iteration", ylab = "loss", ylim = c(0,
    max(loss1,loss2,loss3,loss4,loss5) + 0.01), type = "p", col = "black", cex = 0.7)
title("Convergence Comparision Between Learning Algorithms")
points(iteration, loss2, col = "red", pch = 2, cex = 0.7)
points(iteration, loss3, col = "blue", pch = 3, cex = 0.7)
points(iteration, loss4, col = "green", pch = 4, cex = 0.7)
points(iteration, loss5, col = "magenta", pch = 5, cex = 0.7)

legend("topright", c("SGD", "Adagrad", "RMSprop", "Momentum", "NAG"),
    col = c("black", "red", "blue", "green", "magenta"),pch = c(1,2,3,4,5))

save.image()

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
