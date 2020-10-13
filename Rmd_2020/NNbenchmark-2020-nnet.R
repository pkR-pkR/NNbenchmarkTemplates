## ----message=FALSE, warning=FALSE-------------------------------------------------------------------
library(NNbenchmark)
library(kableExtra)
options(scipen = 999)


## ---------------------------------------------------------------------------------------------------
NNdataSummary(NNdatasets)


## ---------------------------------------------------------------------------------------------------
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

#library(nnet)
nnet.method <- "none"
hyperParams.nnet <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.nnet <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
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
if(FALSE)
res <- trainPredict_1data(1, nnet.method, "NNtrain.nnet", "hyperParams.nnet", "NNpredict.nnet", 
                               NNsummary, "NNclose.nnet", NA, nnet.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="nnet", pkgfun="nnet", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----------------------
res <- trainPredict_1pkg(1:12, pkgname = "nnet", pkgfun = "nnet", nnet.method,
  prepareZZ.arg = nnet.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## ---------------------------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

