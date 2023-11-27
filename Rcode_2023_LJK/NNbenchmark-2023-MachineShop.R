cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(MachineShop, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/MachineShop"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/MachineShop/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(MachineShop)
MachineShop.method <- "none"
hyperParams.MachineShop <- function(...) 
{
  return (list(iter=maxit2ndorder, trace=FALSE, linout=TRUE))
}
NNtrain.MachineShop <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) 
{
  
  hyper_params <- do.call(hyperParams, list(...))
  trace <- hyper_params$trace
  maxit <- hyper_params$iter
  linout <- hyper_params$linout #linearoutpputunit
  myNN <- MachineShop::NNetModel(size = neur, linout = linout, maxit = maxit,
                                 trace=trace)
  MachineShop::fit(formula, data = dataxy, model = myNN)
  
}
NNpredict.MachineShop <- function(object, x, ...)
  as.numeric(predict(object, newdata=x, type="response"))
NNclose.MachineShop <- function()
  if("package:MachineShop" %in% search())
    detach("package:MachineShop", unload=TRUE)
MachineShop.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, MachineShop.method, "NNtrain.MachineShop", "hyperParams.MachineShop", "NNpredict.MachineShop", 
                              NNsummary, "NNclose.MachineShop", NA, MachineShop.prepareZZ, nrep=5,
                              echo=FALSE, doplot=FALSE, echoreport=0,
                              pkgname="MachineShop", pkgfun="fit", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "MachineShop", pkgfun = "fit", MachineShop.method,
                           prepareZZ.arg = MachineShop.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


