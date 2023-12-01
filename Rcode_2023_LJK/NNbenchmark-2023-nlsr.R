cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(nlsr, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/nlsr"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/nlsr/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(nlsr)
nlsr.method <- "none"
hyperParams.nlsr <- function(...) 
{
  return (list(iter = maxit2ndorder, sdnormstart = 0.1))
}
NNtrain.nlsr <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) 
{
  
  hyper_params <- do.call(hyperParams, list(...))
  start <- round(rnorm(NNparam, sd = hyper_params$sdnormstart), 4)
  names(start)  <- paste0("b", 1:NNparam)
  
  NNreg <- nlsr::nlxb(NNfullformula, data = dataxy, start = start, control = list(femax = hyper_params$iter))
  return(NNreg)
}
NNpredict.nlsr <- function(object, x, ...)
  as.numeric(predict(object, x))
NNclose.nlsr <- function()
  if("package:nlsr" %in% search())
    detach("package:nlsr", unload=TRUE)
nlsr.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, nlsr.method, "NNtrain.nlsr", "hyperParams.nlsr", "NNpredict.nlsr", 
                              NNsummary, "NNclose.nlsr", NA, nlsr.prepareZZ, nrep=5,
                              echo=FALSE, doplot=FALSE, echoreport=0,
                              pkgname="nlsr", pkgfun="nlxb", csvfile=TRUE, rdafile=TRUE, odir=odir)
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "nlsr", pkgfun = "nlxb", nlsr.method,
                           prepareZZ.arg = nlsr.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
#}


