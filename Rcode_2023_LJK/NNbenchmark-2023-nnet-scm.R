cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark)
require(nnet)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/nnet"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/nnet/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(nnet)
nnet.method <- "none"
hyperParams.nnet <- function(...) {
  return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.nnet <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) 
{
  
  hyper_params <- do.call(hyperParams, list(...))
  
  NNreg <- nnet::nnet(x, y, size = neur, linout = TRUE, maxit = hyper_params$iter, trace=hyper_params$trace)
  return(NNreg)
}
NNpredict.nnet <- function(object, x, ...)
  predict(object, newdata=x)
NNclose.nnet <- function()
  if("package:nnet" %in% search())
    detach("package:nnet", unload=TRUE)
nnet.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)


## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "nnet", pkgfun = "nnet", nnet.method,
                           prepareZZ.arg = nnet.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
  
)
print(t1)
if(length(useless) > 0)
{
  print(head(useless))
  print(tail(useless))
}
#}
