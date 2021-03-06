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

#library(MachineShop)
MachineShop.method <- "none"
hyperParams.MachineShop <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE, linout=TRUE))
}
NNtrain.MachineShop <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    trace <- hyper_params$trace
    maxit <- hyper_params$iter
    linout <- hyper_params$linout #linearoutpputunit
    myNN <- MachineShop::NNetModel(size = neur, linout = linout, maxit = maxit,
                                   trace=trace)
    MachineShop::fit(formula, data = dataxy, model = myNN)
    
}
NNpredict.MachineShop <- function(object, x, ...)
    as.numeric(predict(object, newdata=x, type="response"))
NNclose.MachineShop <- function()
  if("package:MachineShop" %in% search())
    detach("package:MachineShop", unload=TRUE)
MachineShop.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, MachineShop.method, "NNtrain.MachineShop", "hyperParams.MachineShop", "NNpredict.MachineShop", 
                               NNsummary, "NNclose.MachineShop", NA, MachineShop.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="MachineShop", pkgfun="fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----------------------
res <- trainPredict_1pkg(1:12, pkgname = "MachineShop", pkgfun = "fit", MachineShop.method,
  prepareZZ.arg = MachineShop.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## ---------------------------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

