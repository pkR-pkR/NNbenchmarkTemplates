## ----message=FALSE, warning=FALSE------------------------------------------------
library(NNbenchmark)
library(kableExtra)
options(scipen = 999)


## --------------------------------------------------------------------------------
NNdataSummary(NNdatasets)


## --------------------------------------------------------------------------------
if(dir.exists("D:/GSoC2020/Results/2020run04/"))
{  
  odir <- "D:/GSoC2020/Results/2020run04/"
}else if(dir.exists("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"))
{  
  odir <- "~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"
}else
  odir <- "~"

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
if(FALSE)
res <- trainPredict_1data(1, minpack.lm.method, "NNtrain.minpack.lm", "hyperParams.minpack.lm", "NNpredict.minpack.lm", 
                               NNsummary, "NNclose.minpack.lm", NA, minpack.lm.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="minpack.lm", pkgfun="nlsLM", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "minpack.lm", pkgfun = "nlsLM", minpack.lm.method,
  prepareZZ.arg = minpack.lm.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

