cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(validann, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/validann"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/validann/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(validann)
validann.method <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
hyperParams.validann <- function(optim_method, ...) {
  
  if (optim_method == "Nelder-Mead")  {iter <- maxit1storderB} 
  if (optim_method == "BFGS")         {iter <- maxit2ndorder}
  if (optim_method == "CG")           {iter <- maxit1storderA}
  if (optim_method == "L-BFGS-B")     {iter <- maxit2ndorder}
  if (optim_method == "SANN")         {iter <- maxit1storderA}
  
  out <- list(iter = iter)
  return (out)
}
NNtrain.validann <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  iter <- hyper_params$iter
  method <- hyper_params$method
  
  NNreg <- validann::ann(x, y, size = neur, 
                         method = optim_method, maxit = iter)
  
  return (NNreg)
}
NNpredict.validann <- function(object, x, ...)
  predict(object, x)
NNclose.validann <- function()
  if("package:validann" %in% search())
    detach("package:validann", unload=TRUE)
validann.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, validann.method, "NNtrain.validann", "hyperParams.validann", "NNpredict.validann", 
                              NNsummary, "NNclose.validann", NA, validann.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                              pkgname="validann", pkgfun="ann", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "validann", pkgfun = "ann", validann.method,
                           prepareZZ.arg = validann.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
#}


