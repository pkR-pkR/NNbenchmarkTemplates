## ---- include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------
library(NNbenchmark)


## --------------------------------------------------------------------------------
if(dir.exists("D:/GSoC2020/Results/2020run05/"))
{  
  odir <- "D:/GSoC2020/Results/2020run05/"
}else if(dir.exists("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"))
{  
  odir <- "~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"
}else
  odir <- "~"


## --------------------------------------------------------------------------------
controlpredplot <- function(data, pred, plot.nr, plot.nc, plot.mar=c(4,4,2,1), plot.nobs=1000, var2round,...)
{
  if(var2round)
  {
    par(mfrow=c(plot.nr,plot.nc), mar=plot.mar)
    for(j in 1:(plot.nr*plot.nc))
    {
      Vjb <- round(data[,paste0("x",j)], 2)
      plot(sort(unique(Vjb)), tapply(data$y, Vjb, mean), 
           xlab=paste0("rounded variable ", j), ylab="y", ...)
      lines(sort(unique(Vjb)), tapply(pred, Vjb, mean), col=2)
    }
  }else
  {
    n <- min(plot.nobs, NROW(data)) 
    par(mfrow=c(plot.nr,plot.nc), mar=plot.mar)
    for(j in 1:(plot.nr*plot.nc))
    {
      plot(data[1:n,paste0("x",j)], data$y[1:n], 
           xlab=paste0("original variable ", j), ylab="y",...)
      points(data[1:n,paste0("V",j)], pred[1:n], col=2, pch=3)
    }
  }
  
}


## --------------------------------------------------------------------------------
bWoodfullfile <- grep("Wood", list.files(odir, pattern="RData", full.names=TRUE), value=TRUE)
bWoodfile <- grep("Wood", list.files(odir, pattern="RData"), value=TRUE)

bWoodtitle <- substr(bWoodfile, 9, nchar(bWoodfile)-6)
bWoodgraph <- substr(bWoodfile, 1, nchar(bWoodfile)-6)


for(i in 1:length(bWoodfullfile))
{
  load(file=bWoodfullfile[i])
  
  cat(bWoodfile[i], "\n")
  
  jstar <- which.min(sapply(1:NCOL(Ypred), function(j) NNsummary(Ypred[,j], bWoodN1$y, 0)["RMSE"]))
  Ystar <- Ypred[,jstar]
  
  pdf(paste0(odir, "/", bWoodgraph[i], ".pdf"), width = 10, height = 4)
  controlpredplot(bWoodN1, Ystar, 2, 3, var2round = TRUE)
  title(main=bWoodtitle[i])
  dev.off()
}

