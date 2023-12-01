cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(EnsembleBase, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/EnsembleBase"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/EnsembleBase/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(EnsembleBase)
EnsembleBase.method <- "none"
hyperParams.EnsembleBase <- function(optim_method, ...) {
  out <- list(iter = maxit2ndorder, decay = 0)
  return (out)
}
NNtrain.EnsembleBase <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) 
{
  hyper_params <- do.call(hyperParams.EnsembleBase, list(EnsembleBase.method))
  
  iter <- hyper_params$iter ; decay <- hyper_params$decay
  
  NNreg <- EnsembleBase::Regression.Batch.Fit(make.configs("nnet", config.df = expand.grid(decay=decay,size=c(neur),maxit=iter)), formula, dataxy, ncores = 1)
  
  return (NNreg)
}
NNpredict.EnsembleBase <- function(object, x, ...)
  predict(object, x)
NNclose.EnsembleBase <- function()
  if("package:EnsembleBase" %in% search())
    detach("package:EnsembleBase", unload=TRUE)
EnsembleBase.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, EnsembleBase.method, "NNtrain.EnsembleBase", "hyperParams.EnsembleBase", "NNpredict.EnsembleBase", 
                              NNsummary, "NNclose.EnsembleBase", NA, EnsembleBase.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                              pkgname="EnsembleBase", pkgfun="EnsembleBase", csvfile=TRUE, rdafile=TRUE, odir=odir, 
                              lib.loc = "/home/dutangc/Rpersolib")
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "EnsembleBase", pkgfun = "Regression.Batch.Fit", EnsembleBase.method,
                           prepareZZ.arg = EnsembleBase.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE,
                           lib.loc = "/home/dutangc/Rpersolib"))
)
print(t1)
#}


