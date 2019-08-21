
## =================
## PACKAGE neuralnet
## =================
# source("package-neuralnet.R")
library(neuralnet)

### ATTACH
ZZ <- prepareZZ(Z, xdmv = "d", ydmv= "d", scale = FALSE)
attach(ZZ)


###  ’backprop’, ’rprop+’, ’rprop-’, ’sag’, or ’slr’
### ALGO "rprop+"  => DOES NOT WORK !!
algo  <- "rprop+"
lr    <- NULL
descr <- "neuralnet::neuralnet_rprop+"

descr  <- paste(dset, descr, sep = "_")
plotNN(xory, y0, uni, TF, main = descr)
Ypred <- list()
Rmse  <- numeric(length = nrep)
for(i in 1:nrep){
    event <- paste(dset, descr, sprintf("_%.2d", i), sep = "_")
    timer$start(event)
    ## ADJUST THESE TWO LINES TO THE PACKAGE::ALGORITHM
    NNreg  <- neuralnet(formula = fmla, data = Z, hidden = neur, 
                        algorithm = algo, learningrate = lr, act.fct = "tanh")
    y_pred <- ym0 + ysd0*predict(NNreg, newdata = x, all.units = FALSE)
    ##
    Ypred[[i]] <- y_pred
    Rmse[i]  <- sqrt(sum((y_pred - y0)^2)/(length(y_pred)))
    timer$stop(event, RMSE = round(Rmse[i], 4))
    lipoNN(xory, y_pred, uni, TF, col = i)
}
best <- which(Rmse == min(Rmse))[1] ; best
# lipoNN(xory, Ypred[[best]], uni, TF, col = 4, lwd = 4, pch = 3, cex = 1.4)
# getTimer(timer)



###  ’backprop’, ’rprop+’, ’rprop-’, ’sag’, or ’slr’
### ALGO "backprop"  => DOES NOT WORK !!
algo  <- "backprop"
lr    <- 0.001
descr <- "neuralnet::neuralnet_backprop"

descr  <- paste(dset, descr, sep = "_")
plotNN(xory, y0, uni, TF, main = descr)
Ypred <- list()
Rmse  <- numeric(length = nrep)
for(i in 1:nrep){
    event     <- paste(dset, descr, sprintf("_%.2d", i), sep = "_")
    timer$start(event)
    ## ADJUST THESE TWO LINES TO THE PACKAGE::ALGORITHM
    NNreg  <- neuralnet(formula = fmla, data = Z, hidden = neur, 
                        algorithm = algo, learningrate = lr, act.fct = "tanh")
    y_pred <- ym0 + ysd0*predict(NNreg, newdata = x, all.units = FALSE)
    ##
    Ypred[[i]] <- y_pred
    Rmse[i]  <- sqrt(sum((y_pred - y0)^2)/(length(y_pred)))
    timer$stop(event, RMSE = round(Rmse[i], 4))
    lipoNN(xory, y_pred, uni, TF, col = i)
}
best <- which(Rmse == min(Rmse))[1] ; best
lipoNN(xory, Ypred[[best]], uni, TF, col = 4, lwd = 4, pch = 3, cex = 1.4)
# getTimer(timer)


### DETACH
detach(ZZ) 
detach("package:neuralnet", unload=TRUE)
remove(list= ls()[!(ls() %in% donotremove)])
ls()


