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

#library(caret)
caret.method <- "none"
hyperParams.caret <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.caret <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    NNreg <- caret::avNNet(x, y, repeats = 3, size = neur, linout = TRUE, trace=hyper_params$trace, bag = TRUE, maxit = maxit2ndorder)
    return(NNreg)
}
NNpredict.caret <- function(object, x, ...)
    predict(object, newdata=x)
NNclose.caret <- function()
  if("package:caret" %in% search())
    detach("package:caret", unload=TRUE)
caret.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, caret.method, "NNtrain.caret", "hyperParams.caret", "NNpredict.caret", 
                               NNsummary, "NNclose.caret", NA, caret.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="caret", pkgfun="caret", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "caret", pkgfun = "avNNet", caret.method,
  prepareZZ.arg = caret.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

