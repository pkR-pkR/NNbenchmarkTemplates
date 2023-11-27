cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(RSNNS, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/RSNNS"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/RSNNS/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(RSNNS)
RSNNS.method <- c("Rprop","BackpropBatch","BackpropChunk","BackpropMomentum",
                  "BackpropWeightDecay","Quickprop","SCG","Std_Backpropagation")
hyperParams.RSNNS <- function(optim_method, ...) {
  
  if (optim_method == "Std_Backpropagation")   {iter <- maxit1storderA} 
  if (optim_method == "BackpropBatch")         {iter <- maxit1storderB}
  if (optim_method == "BackpropChunk")         {iter <- maxit1storderA}
  if (optim_method == "BackpropMomentum")      {iter <- maxit1storderA}
  if (optim_method == "BackpropWeightDecay")   {iter <- maxit1storderA}
  if (optim_method == "Rprop")                 {iter <- maxit1storderA}
  if (optim_method == "Quickprop")             {iter <- maxit1storderB}
  if (optim_method == "SCG")                   {iter <- maxit1storderA}
  
  out <- list(iter = iter)
  return(out)
}
NNtrain.RSNNS <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  if (optim_method == "Std_Backpropagation" || optim_method == "BackpropBatch"){
    NNreg <- RSNNS::mlp(x, y, size = hidden_neur, learnFunc = optim_method, learnFuncParams = c(0.1), #lr 
                        maxit = hyper_params$iter, linOut = TRUE)
  } else {
    NNreg <- RSNNS::mlp(x, y, size = hidden_neur, learnFunc = optim_method,
                        maxit = hyper_params$iter, linOut = TRUE)
  }
  
  return (NNreg)
}
NNpredict.RSNNS <- function(object, x, ...)
  predict(object, x)
NNclose.RSNNS <- function()
  if("package:RSNNS" %in% search())
    detach("package:RSNNS", unload=TRUE)
RSNNS.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, RSNNS.method, "NNtrain.RSNNS", "hyperParams.RSNNS", "NNpredict.RSNNS", 
                                   NNsummary, "NNclose.RSNNS", NA, RSNNS.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                                   pkgname="RSNNS", pkgfun="mlp", rdafile=TRUE, odir=odir)
    
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "RSNNS", pkgfun = "mlp", RSNNS.method,
                           prepareZZ.arg = RSNNS.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


