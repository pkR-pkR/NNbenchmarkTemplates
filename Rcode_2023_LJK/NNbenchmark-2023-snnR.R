cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(snnR, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/snnR"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/snnR/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(snnR)
snnR.method <- "none"
hyperParams.snnR <- function(optim_method, ...) {
  out <- list(iter = maxit2ndorder)
  return (out)
}
NNtrain.snnR <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams,...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  NNreg <- snnR::snnR(x, y, nHidden = as.matrix(hidden_neur), 
                      iteramax = hyper_params$iter, verbose=FALSE)
  return (NNreg)
}
NNpredict.snnR <- function(object, x, ...)
  predict(object, x)
NNclose.snnR <- function()
  if("package:snnR" %in% search())
    detach("package:snnR", unload=TRUE)
snnR.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, simpleNeural.method, "NNtrain.snnR", "hyperParams.snnR", "NNpredict.snnR", 
                                   NNsummary, "NNclose.snnR", NA, simpleNeural.prepareZZ, nrep=5, echo=FALSE, doplot=TRUE,
                                   pkgname="snnR", pkgfun="snnR", rdafile=TRUE, odir=odir, echoreport=1)
    
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "snnR", pkgfun = "snnR", snnR.method,
                           prepareZZ.arg = snnR.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
  
)
print(t1)
#}


