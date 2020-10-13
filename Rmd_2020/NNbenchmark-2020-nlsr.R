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

#library(nlsr)
nlsr.method <- "none"
hyperParams.nlsr <- function(...) {
    return (list(iter = maxit2ndorder, sdnormstart = 0.1))
}
NNtrain.nlsr <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) {
    
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
if(FALSE)
res <- trainPredict_1data(1, nlsr.method, "NNtrain.nlsr", "hyperParams.nlsr", "NNpredict.nlsr", 
                               NNsummary, "NNclose.nlsr", NA, nlsr.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="nlsr", pkgfun="nlxb", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----------------------
res <- trainPredict_1pkg(1:12, pkgname = "nlsr", pkgfun = "nlxb", nlsr.method,
  prepareZZ.arg = nlsr.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## ---------------------------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

