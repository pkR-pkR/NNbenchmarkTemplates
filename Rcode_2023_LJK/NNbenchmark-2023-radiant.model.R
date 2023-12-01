cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(radiant.model, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/radiant.model"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/radiant.model/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(radiant.model)
radiant.model.method <- "none"
hyperParams.radiant.model <- function(...) {
  return (list(iter=maxit2ndorder, type="regression", decay=0))
}
NNtrain.radiant.model <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(...))
  
  radiant.model::nn(dataxy, rvar = "y", evar = attr(terms(formula), "term.labels"),
                    type = hyper_params$type, size = neur, maxit = hyper_params$iter,
                    decay = hyper_params$decay)
  
}
NNpredict.radiant.model <- function(object, x, ...)
  predict(object, pred_data=as.data.frame(x))$Prediction
NNclose.radiant.model <- function()
  if("package:radiant.model" %in% search())
    detach("package:radiant.model", unload=TRUE)
radiant.model.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, radiant.model.method, "NNtrain.radiant.model", "hyperParams.radiant.model", "NNpredict.radiant.model", 
                              NNsummary, "NNclose.radiant.model", NA, radiant.model.prepareZZ, nrep=5,
                              echo=FALSE, doplot=FALSE, echoreport=0,
                              pkgname="radiant.model", pkgfun="nn", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "radiant.model", pkgfun = "nn", radiant.model.method,
                           prepareZZ.arg = radiant.model.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
#}


