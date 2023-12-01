cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(rminer, lib.loc = "/home/dutangc/Rpersolib")

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/rminer"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/rminer/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(rminer)
rminer.method <- "none"
hyperParams.rminer <- function(...) {
  return (list(task="reg", iter=maxit2ndorder))
}
NNtrain.rminer <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(...))
  
  rminer::fit(formula, data = dataxy, model = "mlp", task = hyper_params$task, 
              size = neur, maxit = hyper_params$iter)
}
NNpredict.rminer <- function(object, x, ...)
  as.numeric(rminer::predict(object, newdata=as.data.frame(x)))
NNclose.rminer <- function()
  if("package:rminer" %in% search())
    detach("package:rminer", unload=TRUE)
rminer.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- trainPredict_1data(1, rminer.method, "NNtrain.rminer", "hyperParams.rminer", "NNpredict.rminer", 
                              NNsummary, "NNclose.rminer", NA, rminer.prepareZZ, nrep=2,
                              echo=FALSE, doplot=FALSE, echoreport=0,
                              pkgname="rminer", pkgfun="fit", csvfile=TRUE, rdafile=TRUE, odir=odir)
      )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "rminer", pkgfun = "fit", rminer.method,
                           prepareZZ.arg = rminer.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
  
  
)
print(t1)
#}


