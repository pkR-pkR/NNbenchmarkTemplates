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

#library(validann)
validann.method <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
hyperParams.validann <- function(optim_method, ...) {
    
    if (optim_method == "Nelder-Mead")  {iter <- maxit1storderB} 
    if (optim_method == "BFGS")         {iter <- maxit2ndorder}
    if (optim_method == "CG")           {iter <- maxit1storderA}
    if (optim_method == "L-BFGS-B")     {iter <- maxit2ndorder}
    if (optim_method == "SANN")         {iter <- maxit1storderA}
  
    out <- list(iter = iter)
    return (out)
}
NNtrain.validann <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    method <- hyper_params$method
    
    NNreg <- validann::ann(x, y, size = neur, 
                           method = optim_method, maxit = iter)
    
    return (NNreg)
}
NNpredict.validann <- function(object, x, ...)
  predict(object, x)
NNclose.validann <- function()
  if("package:validann" %in% search())
    detach("package:validann", unload=TRUE)
validann.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, validann.method, "NNtrain.validann", "hyperParams.validann", "NNpredict.validann", 
                               NNsummary, "NNclose.validann", NA, validann.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="validann", pkgfun="ann", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----------------------
res <- trainPredict_1pkg(1:12, pkgname = "validann", pkgfun = "ann", validann.method,
  prepareZZ.arg = validann.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## ---------------------------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "Nelder-Mead")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "BFGS")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "CG")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "L-BFGS-B")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,5,], c(3,1), min), caption = "SANN")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

