cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(deepnet, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/deepnet"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/deepnet/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(deepnet)
deepnet.method <- "BP"
hyperParams.deepnet <- function(optim_method, ...) 
{
  iter <- maxit1storderA
  lr <- 0.8
  dropout <- 0
  momentum <- 0.95
  hidden_activation <- "sigm"
  
  out <- list(iter = iter, lr = lr, momentum = momentum, hidden_activation = hidden_activation, dropout = dropout)
  
  return (out)
}

NNtrain.deepnet <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) 
{
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  iter <- hyper_params$iter
  lr <- hyper_params$lr
  dropout <- hyper_params$dropout
  momentum <- hyper_params$momentum
  hidden_activation <- hyper_params$hidden_activation
  dropout <- hyper_params$dropout
  
  NNreg <- deepnet::nn.train(x = x, y = y, 
                             hidden = c(hidden_neur), 
                             activationfun = hidden_activation, 
                             learningrate = lr, 
                             output = 'linear', 
                             numepochs = iter, 
                             hidden_dropout = dropout, 
                             momentum = momentum)
  return (NNreg)
}
NNpredict.deepnet <- function(object, x, ...)
  deepnet::nn.predict(nn = object, x = x)
NNclose.deepnet <- function()
  if("package:deepnet" %in% search())
    detach("package:deepnet", unload=TRUE)
deepnet.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, CaDENCE.method, "NNtrain.deepnet", "hyperParams.deepnet", "NNpredict.deepnet", 
                                   NNsummary, "NNclose.deepnet", NA, deepnet.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                                   pkgname="deepnet", pkgfun="nn.train", rdafile=TRUE, odir=odir)
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "deepnet", pkgfun = "nn.train", deepnet.method,
                           prepareZZ.arg = deepnet.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
#}


