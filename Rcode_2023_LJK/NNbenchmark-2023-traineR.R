cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(traineR, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/traineR"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/traineR/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(traineR)
traineR.method <- "none"
hyperParams.traineR <- function(...) {
  return (list(iter=maxit2ndorder))
}
NNtrain.traineR <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
  hyper_params <- do.call(hyperParams, list(...))
  NNreg <- traineR::train.nnet(formula = formula, data = dataxy, size = neur, maxit = hyper_params$iter, linout = TRUE)
  return(NNreg)
}
NNpredict.traineR <- function(object, x, dataxy, ...){
  object$fitted.values 
}
NNclose.traineR <- function()
  if("package:traineR" %in% search())
    detach("package:traineR", unload=TRUE)
traineR.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, traineR.method, "NNtrain.traineR", "hyperParams.traineR", "NNpredict.traineR", 
                              NNsummary, "NNclose.traineR", NA, traineR.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                              pkgname="traineR", pkgfun="traineR", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "traineR", pkgfun = "train.nnet", traineR.method,
                           prepareZZ.arg = traineR.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
  
)
print(t1)
#}


