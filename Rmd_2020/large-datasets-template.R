## ----setup, message=FALSE, warning=FALSE-----------------------------------------
library(NNbenchmark)
library(kableExtra)
library(knitr)
options(scipen = 999)


## --------------------------------------------------------------------------------
NNdataSummary(NNbigdatasets)
ht(bWoodN1)


## --------------------------------------------------------------------------------
if(dir.exists("D:/GSoC2020/Results/2020run05/"))
{  
  odir <- "D:/GSoC2020/Results/2020run05/"
}else if(dir.exists("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"))
{  
  odir <- "~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"
}else
  odir <- "."

nrep <- 10
maxit2ndorder  <-    400
maxit1storderA <-   2000
maxit1storderB <-  10000
maxit1storderC <- 100000



## --------------------------------------------------------------------------------
pkglist <- c("nlsr", "rminer", "nnet", "validann", "MachineShop")

for(pkg in pkglist)
{
  pkgname.R <- paste0("NNbenchmark-2020-", pkg, ".R")
  pkgname.Rmd <- paste0("NNbenchmark-2020-", pkg, ".Rmd")
  configname.R <- paste0("NNbenchmark-2020-", pkg, "-Config.R")
  if(!file.exists(configname.R))
  {
    if(!file.exists(pkgname.R))
      purl(pkgname.Rmd)
    
    ConfigFile <- readLines(pkgname.R)
    ConfigFile.beg <- 28
    ConfigFile.end <- which(ConfigFile == "if(FALSE)")-1
    if(length(ConfigFile.end) == 0)
      ConfigFile.end <- 48
    
    writeLines(ConfigFile[ConfigFile.beg:ConfigFile.end], configname.R)
  }
  source(configname.R)
}


## ---- message=FALSE, warning=FALSE, fig.height=7, fig.width=14-------------------
methodlist <- list(nlsr.method, 
                   rminer.method,
                   nnet.method, 
                   validann.method[2],
                   MachineShop.method)
pkgfunmat <- rbind(c("nlsr", "nlxb"),
                    c("rminer", "fit"),
                    c("nnet", "nnet"),
                    c("validann","ann"),
                   c("MachineShop", "fit"))
colnames(pkgfunmat) <- c("pkg", "fun")  
trainvect <- paste("NNtrain", pkgfunmat[,"pkg"], sep=".")
hypervect <- paste("hyperParams", pkgfunmat[,"pkg"], sep=".")
predvect <- paste("NNpredict", pkgfunmat[,"pkg"], sep=".")
closevect <- paste("NNclose", pkgfunmat[,"pkg"], sep=".")
startvect <- rep(NA, length(pkgfunmat[,"pkg"]))
startvect[pkgfunmat[,"pkg"] == "h2o"] <- "NNstart.h2o"
preparelist <- list(nlsr.prepareZZ, 
                    rminer.prepareZZ,
                    nnet.prepareZZ, 
                    validann.prepareZZ,
                    MachineShop.prepareZZ)
names(preparelist) <- pkgfunmat[,"pkg"]

resall <- trainPredict_1data(dset=13, method=methodlist, train=trainvect, hyper=hypervect,
                     pred=predvect, summary=NNsummary, close=closevect, 
                     start=startvect, prepare=preparelist, nrep=nrep, echo=FALSE, doplot=TRUE,
                     pkgname=pkgfunmat[,"pkg"], pkgfun=pkgfunmat[,"fun"], 
                     csvfile = TRUE, rdafile = TRUE, odir = odir)

colnames(resall) <- pkgfunmat[,"pkg"]
rownames(resall) <- 
  paste0(rep(c("RMSE", "MSE", "MAE", "WAE", "time"), length=5*nrep), ".", rep(1:nrep, each=5))

resmin <- sapply(1:NCOL(resall),
       function(j)
       {
         idx.min <- which.min(resall[grep("RMSE.", rownames(resall)),j])
          idx <- (idx.min-1)*5+1:5
          resall[idx, j]
       })
colnames(resmin) <- pkgfunmat[,"pkg"]
rownames(resmin) <- c("RMSE", "MSE", "MAE", "WAE", "time")
kable(resmin) 



which.median <- function(x)
{
  n <- length(x)
  if(n %% 2 == 1)
    which(rank(x) == n/2+1)
  else
    which(rank(x) == n/2)
}

# resmedian <- sapply(1:NCOL(resall),
#        function(j)
#        {
#          idx.min <- which.median(resall[grep("RMSE.", rownames(resall)),j])
#           idx <- (idx.min-1)*5+1:5
#           resall[idx, j]
#        })
# colnames(resmedian) <- pkgfunmat[,"pkg"]
# rownames(resmedian) <- c("RMSE", "MSE", "MAE", "WAE", "time")
# kable(resmedian)     


