cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark)
require(minpack.lm)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/minpack.lm"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/minpack.lm/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(minpack.lm)
minpack.lm.method <- "none"
hyperParams.minpack.lm <- function(...) {
  return (list(iter=maxit2ndorder))
}
NNtrain.minpack.lm <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) {
  
  hyper_params <- do.call(hyperParams, list(...))
  
  minpack.lm::nlsLM(NNfullformula, data = dataxy,
                    control = list(maxiter = hyper_params$iter))
}
NNpredict.minpack.lm <- function(object, x, ...)
  predict(object, newdata=as.data.frame(x))
NNclose.minpack.lm <- function()
  if("package:minpack.lm" %in% search())
    detach("package:minpack.lm", unload=TRUE)
minpack.lm.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)


## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "minpack.lm", pkgfun = "nlsLM", minpack.lm.method,
                           prepareZZ.arg = minpack.lm.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
if(length(useless) > 0)
{
  print(head(useless))
  print(tail(useless))
}
#}
