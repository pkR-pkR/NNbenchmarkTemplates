

## ====================================================
## TEST DES FICHIERS DE WOOD 
## ====================================================
library(NNbenchmark)
rm(list=ls(all=TRUE))
setwd("D:/DevGSoC/datasetsWood") ; getwd()

# load("mWoodN1.rda")  # LOI NORMALE
# load("mWoodG1.rda")  # LOI GAMMA
# load("mWoodIG1.rda") # LOI INVERSE GAMMA


load("mWoodN1_5.rda")  # LOI NORMALE
# load("mWoodG1_5.rda")  # LOI GAMMA
# load("mWoodIG1_5.rda") # LOI INVERSE GAMMA
ls()
data <- mWoodN1
dim(data)
ht(data)


# data$y <- sqrt(data$y)
# dim(data)
# ht(data)



## ==================================================
## LES 9 PREMIÈRES VARIABLES SUR UN PETIT ÉCHANTILLON
## ==================================================
data_s <- data[sample(nrow(data), 1000), ]
round(ht(data_s), 3) ; dim(data_s)


op <- par(mfrow = c(3,2), omi = c(0,0,1.5,0.2)/5, tcl = -0.3, 
          mai = c(1.8,2.7,1,0.5)/5, mgp = c(3,0.5,0), 
          ps = 10, cex = 1, cex.main = 1, las = 1) 
for (V in colnames(data)) {
data2 <- data[order(data[,V]), c("y",V)]
plot(data2[,V], data2[,"y"], type = "b")
# plot(data2[,V], data2[,"y"]^0.5, type = "b")
# plot(data2[,V], log(1+data2[,"y"]), type = "b")
}
par(op)



## ============
## brnn::brnn()
## ============
library(brnn)
system.time(breg <- brnn(y ~ ., data = data, neurons = 10))
with(data, plot(y, predict(breg), las = 1, main = "full"))
abline(a=0, b=1)
data.frame(ypred = round(ht(predict(breg)), 3))

system.time(bregs <- brnn(y ~ ., data = data_s, neurons = 10))
with(data_s, plot(y, predict(bregs), las = 1, main = "sample"))
abline(a=0, b=1)
data.frame(ypred = round(ht(predict(bregs)), 3))


controlpredplot <- function(data, pred, plot.nr, plot.nc, plot.mar=c(4,4,2,1), plot.nobs=1000, var2round,...)
{
  if(var2round)
  {
    par(mfrow=c(plot.nr,plot.nc), mar=plot.mar)
    for(j in 1:(plot.nr*plot.nc))
    {
      Vjb <- round(data[,paste0("V",j)], 2)
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
      plot(data[1:n,paste0("V",j)], data$y[1:n], 
           xlab=paste0("original variable ", j), ylab="y",...)
      points(data[1:n,paste0("V",j)], pred[1:n], col=2, pch=3)
    }
  }
  
}

ypred <- predict(breg)
controlpredplot(mWoodN1, ypred, 2, 3, var2round=TRUE)
controlpredplot(mWoodN1, ypred, 2, 3, var2round=FALSE)



