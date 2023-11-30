cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(CaDENCE, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/CaDENCE"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/CaDENCE/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(CaDENCE)
CaDENCE.method <- c("optim", "psoptim", "Rprop")
hyperParams.CaDENCE <- function(optim_method, ...) 
{
  if (optim_method == "optim")    {iter <- maxit2ndorder;  epsilon <- 0.01} 
  if (optim_method == "psoptim")  {iter <- maxit1storderA; epsilon <- 0.01}
  if (optim_method == "Rprop")    {iter <- maxit1storderA; epsilon <- 0.01}
  
  out <- list(iter = iter, epsilon = epsilon)
  return (out)
}

NNtrain.CaDENCE <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) 
{
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  iter <- hyper_params$iter ; epsilon <- hyper_params$epsilon
  
  NNreg <- CaDENCE::cadence.fit(x = x, y = y, 
                                iter.max = iter, 
                                n.hidden = neur, 
                                hidden.fcn = tanh, 
                                method = optim_method, 
                                n.trials = 1, 
                                trace = 0, 
                                maxit.Nelder = 1, 
                                f.cost = cadence.cost,
                                distribution = list(density.fcn = dnorm,
                                                    parameters = c("mean", "sd"),
                                                    parameters.fixed = NULL,
                                                    output.fcns = c(identity, exp)),
                                epsilon = epsilon)
  return(NNreg)
}
NNpredict.CaDENCE <- function(object, x, ...)
  CaDENCE::cadence.predict(x = x, fit = object)[,1]
NNclose.CaDENCE <- function()
  if("package:CaDENCE" %in% search())
    detach("package:CaDENCE", unload=TRUE)
CaDENCE.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, CaDENCE.method, "NNtrain.CaDENCE", "hyperParams.CaDENCE", "NNpredict.CaDENCE", 
                              NNsummary, "NNclose.CaDENCE", NA, CaDENCE.prepareZZ, nrep=2, echo=FALSE, doplot=FALSE,
                              pkgname="CaDENCE", pkgfun="cadence.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "CaDENCE", pkgfun = "cadence.fit", CaDENCE.method,
                           prepareZZ.arg = CaDENCE.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


