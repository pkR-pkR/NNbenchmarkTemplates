cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(qrnn, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/qrnn"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/qrnn/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(qrnn)
qrnn.method <- "none"
hyperParams.qrnn <- function(optim_method, ...) 
{
  
  iter <- maxit2ndorder
  init.range = c(-0.1, 0.1, -0.1, 0.1)
  
  out <- list(iter = iter, init.range=init.range)
  return (out)
}
NNtrain.qrnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) 
{
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  NNreg <- qrnn::qrnn.fit(x, y, n.hidden = neur, 
                          iter.max = hyper_params$iter, n.trials = 1,
                          init.range = hyper_params$init.range, trace=FALSE)
  
  return (NNreg)
}
NNpredict.qrnn <- function(object, x, ...)
  qrnn::qrnn.predict(x, object)
NNclose.qrnn <- function()
  if("package:qrnn" %in% search())
    detach("package:qrnn", unload=TRUE)
qrnn.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, qrnn.method, "NNtrain.qrnn", "hyperParams.qrnn", "NNpredict.qrnn", 
                              NNsummary, "NNclose.qrnn", NA, qrnn.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                              pkgname="qrnn", pkgfun="qrnn.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "qrnn", pkgfun = "qrnn.fit", qrnn.method,
                           prepareZZ.arg = qrnn.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


