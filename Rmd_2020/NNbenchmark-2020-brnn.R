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

#library(brnn)
brnn.method <- "Gauss-Newton"
hyperParams.brnn <- function(optim_method, ...) {
    return(list(iter = maxit2ndorder))
}
NNtrain.brnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
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
if(FALSE)
res <- trainPredict_1data(1, brnn.method, "NNtrain.brnn", "hyperParams.brnn", "NNpredict.brnn", 
                               NNsummary, "NNclose.brnn", NA, brnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="brnn", pkgfun="brnn", csvfile=TRUE, rdafile=TRUE, odir=odir)
if(FALSE)
res <- trainPredict_1mth1data(1, brnn.method[1], "NNtrain.brnn", "hyperParams.brnn", "NNpredict.brnn", 
                               NNsummary, brnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="brnn", pkgfun="brnn", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "brnn", pkgfun = "brnn", brnn.method,
  prepareZZ.arg = brnn.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

