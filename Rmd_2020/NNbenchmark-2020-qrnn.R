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

#library(qrnn)
qrnn.method <- "none"
hyperParams.qrnn <- function(optim_method, ...) {
    
    iter <- maxit2ndorder
    init.range = c(-0.1, 0.1, -0.1, 0.1)
  
    out <- list(iter = iter, init.range=init.range)
    return (out)
}
NNtrain.qrnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    NNreg <- qrnn::qrnn.fit(x, y, n.hidden = neur, 
                     iter.max = hyper_params$iter, n.trials = 1,
                     init.range = hyper_params$init.range, trace=FALSE)
    
    return (NNreg)
}
NNpredict.qrnn <- function(object, x, ...)
  qrnn::qrnn.predict(x, object)
NNclose.qrnn <- function()
  if("package:qrnn" %in% search())
    detach("package:qrnn", unload=TRUE)
qrnn.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, qrnn.method, "NNtrain.qrnn", "hyperParams.qrnn", "NNpredict.qrnn", 
                               NNsummary, "NNclose.qrnn", NA, qrnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="qrnn", pkgfun="qrnn.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "qrnn", pkgfun = "qrnn.fit", qrnn.method,
  prepareZZ.arg = qrnn.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

