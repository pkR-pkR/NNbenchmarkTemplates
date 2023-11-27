cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(monmlp, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/monmlp"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/monmlp/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(monmlp)
monmlp.method <- c("BFGS", "Nelder-Mead")
hyperParams.monmlp <- function(optim_method, ...) 
{
  if (optim_method == "BFGS")         {iter <- maxit2ndorder}
  if (optim_method == "Nelder-Mead")  {iter <- maxit1storderB} 
  return (list(iter=iter, silent=TRUE, scale=TRUE))
}
NNtrain.monmlp <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, ...) 
{
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  iter <- hyper_params$iter
  scale <- hyper_params$scale
  silent <- hyper_params$silent
  
  monmlp::monmlp.fit(x, y, hidden1 = neur, scale.y = scale, silent=silent,
                     method = optim_method, iter.max = iter)
}
NNpredict.monmlp <- function(object, x, ...)
  as.numeric(monmlp::monmlp.predict(x, weights=object))
NNclose.monmlp <- function()
  if("package:monmlp" %in% search())
    detach("package:monmlp", unload=TRUE)
monmlp.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, monmlp.method, "NNtrain.monmlp", "hyperParams.monmlp", "NNpredict.monmlp", 
                              NNsummary, "NNclose.monmlp", NA, monmlp.prepareZZ, nrep=2,
                              echo=FALSE, doplot=FALSE, echoreport=0,
                              pkgname="monmlp", pkgfun="monmlp.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "monmlp", pkgfun = "monmlp.fit", monmlp.method,
                           prepareZZ.arg = monmlp.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


