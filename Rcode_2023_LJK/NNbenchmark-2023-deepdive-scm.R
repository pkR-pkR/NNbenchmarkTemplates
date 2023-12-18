cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark)
require(deepdive)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/deepdive"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/deepdive/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(deepdive)
deepdive.method <- c("gradientDescent","momentum","rmsProp","adam")
hyperParams.deepdive <- function(optim_method, ...) 
{
  
  if (optim_method == "gradientDescent")  {maxiter <- maxit1storderB; eta = 0.4} 
  if (optim_method == "momentum")         {maxiter <- maxit1storderB; eta = 0.8}
  if (optim_method == "rmsProp")          {maxiter <- maxit1storderA; eta = 0.8}
  if (optim_method == "adam")             {maxiter <- maxit1storderA; eta = 0.8}
  
  out <- list(iter = maxiter, method = optim_method, modelType = "regress", eta = eta, print = 1000)
  return (out)
}
NNtrain.deepdive <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, NNfullformula, NNparam,...) 
{
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  NNreg <- deepdive::deepnet(x = x, y = y, c(neur), modelType = hyper_params$modelType,
                             iterations = hyper_params$iter, eta=hyper_params$eta, 
                             optimiser=hyper_params$method, printItrSize=hyper_params$print,
                             showProgress = FALSE, normalise = FALSE)
  
  return (NNreg)
}
NNpredict.deepdive <- function(object, x, ...)
  predict.deepnet(object,newData=x)$ypred
NNclose.deepdive <- function()
  if("package:deepdive" %in% search())
    detach("package:deepdive", unload=TRUE)
deepdive.prepareZZ <- list(xdmv = "d", ydmv = "d", zdm = "d", scale = TRUE)


## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "deepdive", pkgfun = "deepnet", deepdive.method,
                           prepareZZ.arg = deepdive.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
if(length(useless) > 0)
{
  print(head(useless))
  print(tail(useless))
}
#}
