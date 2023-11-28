cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(ANN2, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/ANN2"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/ANN2/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(ANN2)
ANN2.method <- c("sgd", "adam", "rmsprop")
hyperParams.ANN2 <- function(optim_method, ...) 
{
    if (optim_method == "sgd")     { iter <- maxit1storderA; lr <- 0.01} 
    if (optim_method == "adam")    { iter <- maxit1storderA; lr <- 0.01} 
    if (optim_method == "rmsprop") { iter <- maxit1storderA; lr <- 0.01} 
    
    out <- list(iter = iter, lr = lr)
    return(out)
}

NNtrain.ANN2 <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...)
{
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    
    NNreg <- ANN2::neuralnetwork(X = x, y = y, 
                           val.prop = 0, 
                           standardize = FALSE, 
                           hidden.layers = hidden_neur, 
                           regression = TRUE,
                           loss.type = "squared",
                           n.epochs = iter,
                           optim.type = optim_method,
                           learn.rates = lr,
                           verbose = FALSE,
                           random.seed = as.integer(runif(1)*10000000))
    
   return (NNreg)
}
NNpredict.ANN2 <- function(object, x, ...)
    as.numeric(predict(object, x)$predictions)    
NNclose.ANN2 <- function()
  if("package:ANN2" %in% search())
    detach("package:ANN2", unload=TRUE)
ANN2.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------
if(FALSE)
{
  res <- train_and_predict_1data(1, ANN2.method, "NNtrain.ANN2", "hyperParams.ANN2", "NNpredict.ANN2", 
                               NNsummary, "NNclose.ANN2", NA, ANN2.prepareZZ, nrep=2, echo=FALSE, doplot=FALSE,
                               pkgname="ANN2", pkgfun="neuralnetwork", rdafile=TRUE, odir=odir, 
                               lib.loc = "/home/dutangc/Rpersolib")
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "ANN2", pkgfun = "neuralnetwork", ANN2.method,
                           prepareZZ.arg = ANN2.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE, 
                           lib.loc = "/home/dutangc/Rpersolib")
)
print(t1)
#}

