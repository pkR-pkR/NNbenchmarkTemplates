cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(neuralnet, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/neuralnet"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/neuralnet/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(neuralnet)
neuralnet.method <- c("slr", "sag", "rprop-", "rprop+", "backprop")
hyperParams.neuralnet <- function(method, ...) 
{
  return(list(iter=maxit1storderC, threshold=0.2, linear.output=TRUE))
}
NNtrain.neuralnet <- function(x, y, dataxy, formula, hidden_neur, method, hyperParams, ...) 
{
  
  hyper_params <- do.call(hyperParams, list(method, ...))
  
  if(method == "backprop") {learningrate <- 0.001} else {learningrate <- NULL}
  
  neuralnet::neuralnet(formula = formula, data = dataxy, hidden = hidden_neur, algorithm=method,
                       threshold=hyper_params$threshold, linear.output=hyper_params$linear.output,
                       stepmax = hyper_params$iter, startweights = NULL, act.fct = "tanh",
                       learningrate = learningrate)
}
NNpredict.neuralnet <- function(object, x, ...)
  as.numeric(predict(object, newdata = x))

NNclose.neuralnet <- function()
  if("package:neuralnet" %in% search())
    detach("package:neuralnet", unload=TRUE)
neuralnet.prepareZZ <- list(xdmv = "d", ydmv = "d", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, neuralnet.method, "NNtrain.neuralnet", "hyperParams.neuralnet", "NNpredict.neuralnet", 
                                   NNsummary, "NNclose.neuralnet", NA, neuralnet.prepareZZ, nrep=2,
                                   echo=FALSE, doplot=FALSE, echoreport=0,
                                   pkgname="neuralnet", pkgfun="neuralnet", rdafile=TRUE, odir=odir)
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "neuralnet", pkgfun = "neuralnet", neuralnet.method,
                           prepareZZ.arg = neuralnet.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


