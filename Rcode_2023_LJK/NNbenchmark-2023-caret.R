cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(caret, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/caret"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/caret/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(caret)
caret.method <- "none"
hyperParams.caret <- function(...) {
  return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.caret <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(...))
  
  NNreg <- caret::avNNet(x, y, repeats = 3, size = neur, linout = TRUE, trace=hyper_params$trace, bag = TRUE, maxit = maxit2ndorder)
  return(NNreg)
}
NNpredict.caret <- function(object, x, ...)
  predict(object, newdata=x)
NNclose.caret <- function()
  if("package:caret" %in% search())
    detach("package:caret", unload=TRUE)
caret.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, caret.method, "NNtrain.caret", "hyperParams.caret", "NNpredict.caret", 
                              NNsummary, "NNclose.caret", NA, caret.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                              pkgname="caret", pkgfun="caret", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "caret", pkgfun = "avNNet", caret.method,
                           prepareZZ.arg = caret.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


