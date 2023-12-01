cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(brnn, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/brnn"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/brnn/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(brnn)
brnn.method <- "Gauss-Newton"
hyperParams.brnn <- function(optim_method, ...) 
{
  return(list(iter = maxit2ndorder))
}
NNtrain.brnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) 
{
  hyper_params <- do.call(hyperParams.brnn, list(brnn.method))
  iter <- hyper_params$iter
  
  NNreg <- brnn::brnn(x, y, neur, normalize = FALSE, epochs = iter, verbose = FALSE)
  return (NNreg)
}
NNpredict.brnn <- function(object, x, ...)
  predict(object, x)
NNclose.brnn <- function()
  if("package:brnn" %in% search())
    detach("package:brnn", unload=TRUE)
brnn.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, brnn.method, "NNtrain.brnn", "hyperParams.brnn", "NNpredict.brnn", 
                              NNsummary, "NNclose.brnn", NA, brnn.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                              pkgname="brnn", pkgfun="brnn", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "brnn", pkgfun = "brnn", brnn.method,
                           prepareZZ.arg = brnn.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
#}


