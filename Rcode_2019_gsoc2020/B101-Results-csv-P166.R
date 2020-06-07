

## =============================================================
## 2020-06-06 EXTRACT *results.csv FILES AND RANK THE ALGORITHMS
## =============================================================
library(NNbenchmark)
library(dplyr)
library(stringr)
rm(list=ls(all=TRUE))
setwd("D:/GSoC2020/Results/2019run01") ; getwd()


## ===========================================
## DOWNLOAD AND USE THE GOOD results.csv FILES
## ===========================================
## DOWNLOAD THE MODIFIED *.csv FILES (CORRECTED FROM COMMA IN EXCESS) FROM 
## https://github.com/pkR-pkR/NNbenchmarkTemplates/tree/master/results-text-corrected
## AND COPY THEM INTO YOUR DIRECTORY (HERE "D:/DevGSoC/results")
## THEN


## ===================================================
## READ csv FILES. CALCULATE mean, min, med, d51, npfa
## ===================================================
# base_dir  <- "~/DevGSoC/results"
# base_dir  <- "D:/DevGSoC/results"
base_dir  <- "D:/GSoC2020/Results/2019run01"
lf        <- lapply(list.files(base_dir, pattern = "-results.csv", full.names = TRUE), read.csv2)
names(lf) <- names(NNdatasets)
ht(lf)

gfr0 <- lapply(lf, function(dfr) cbind(
					  ds   = str_remove(str_extract(dfr$event, "\\w+_"), "_"),
					  pfa  = str_sub(str_remove(dfr$event, str_extract(dfr$event, "\\w+_")),  1, -4),
					  run  = str_sub(dfr$event, -2, -1),
					  dfr[,c("duration", "RMSE")]
					  ))

gfr <- lapply(gfr0, function(dfr) cbind.data.frame(
  ds = dfr$ds,
  pfa = dfr$pfa,
  run = dfr$run,
  duration = as.numeric(dfr$duration),
  RMSE = as.numeric(dfr$RMSE)
))

yfr <- lapply(gfr, function(dfr) {
			as.data.frame(dfr %>%
			group_by(pfa) %>%
			summarise(duration.mean = mean(duration), 
			          RMSE.min = min(RMSE), 
			          RMSE.med = median(RMSE),
			          RMSE.d51 = median(RMSE) - min(RMSE)
					  )
			)})
yfr <- lapply(yfr, function(dfr) transform(dfr, npfa = 1:nrow(dfr)))
ht9(yfr)



## ===================================================
## CALCULATE RANKS FOR EVERY DATASET AND MERGE RESULTS
## ===================================================
rankRMSEduration <- function(dfr) {
	dfrDURA <- dfr[order(dfr$duration.mean),]
	dfrRMSE <- dfr[order(dfr$RMSE.min, dfr$duration.mean, dfr$RMSE.med),]
	dfrmed  <- dfr[order(dfr$RMSE.med, dfr$RMSE.min, dfr$duration.mean),]
	dfrd51  <- dfr[order(dfr$RMSE.d51),]
	transform(dfr, 
			  duration.rank = order(dfrDURA$npfa),
			  RMSE.rank = order(dfrRMSE$npfa),
			  med.rank  = order(dfrmed$npfa),
			  d51.rank  = order(dfrd51$npfa)
			  )
}
sfr     <- lapply(yfr, rankRMSEduration); sfr
sfrwide <- do.call(cbind, sfr); sfrwide
sfrwide[, grep("pfa", colnames(sfrwide))] # Identical columns


## =================================
## GLOBAL SCORE ON COMBINED DATASETS
## =================================
sfr.duration   <- sfrwide[, c(grep("duration.rank", colnames(sfrwide)))]
duration.score <- rank(apply(sfr.duration, 1, sum), ties.method = "min")
sfr.RMSE       <- sfrwide[, c(grep("RMSE.rank", colnames(sfrwide)))]
RMSE.score     <- rank(apply(sfr.RMSE, 1, sum), ties.method = "min")
sfr.med        <- sfrwide[, c(grep("med.rank", colnames(sfrwide)))]
med.score      <- rank(apply(sfr.med, 1, sum), ties.method = "min")
sfr.d51        <- sfrwide[, c(grep("d51.rank", colnames(sfrwide)))]
d51.score      <- rank(apply(sfr.d51, 1, sum), ties.method = "min")
scoredfr0 <- data.frame(sfr$uNeuroOne[,"pfa",drop=FALSE], 
# scoredfr0 <- data.frame(sfr$uNeuroOne[,c("pfa")], 
						duration.score, 
					    RMSE.score, 
					    med.score,
					    d51.score)
scoredfr0
scoredfr  <- scoredfr0[order(scoredfr0$RMSE.score),]
rownames(scoredfr) <- NULL
scoredfr


## =======================================================
## A TENTATIVE TABLE FOR THE ARTICLE AND THE HTML FILES 
## THE FULL TABLE, ALL AND THE BEST 15 PACKAGES/ALGORITHMS
## =======================================================
scoredfr[1:15, -1]                  


scoredfr$pfa
 # [1] nlsr::nlxb                           minpacklm::nlsLM                     radiantmodel::radiantmodel          
 # [4] rminer::fit                          validann::ann_BFGS                   nnet::nnet                          
 # [7] MachineShop::fit.NNetModel           qrnn::qrnn.fit                       momlp::monmlp.fit_BFGS              
# [10] brnn::brnn_gaussNewton               validann::ann_CG                     validann::ann_L-BFGS-B   
# [13] tsensembler::tsensembler             ANN2::neuralnetwork_adam             AMORE::train_ADAPTgdwm              
# [16] neuralnet::neuralnet_slr             neuralnet::neuralnet_rprop+          neuralnet::neuralnet_rprop-         
# [19] RSNNS::mlp_Rprop                     RSNNS::mlp_SCG                       ANN2::neuralnetwork_rmsprop         
# [22] AMORE::train_ADAPTgd                 ANN2::neuralnetwork_sgd              RSNNS::mlp_BackpropMomentum         
# [25] RSNNS::mlp_BackpropWeightDecay       RSNNS::mlp_Std_Backpropagation       RSNNS::mlp_BackpropChunk            
# [28] deepnet::gradientdescent             momlp::monmlp.fit_Nelder-Mead        RSNNS::mlp_BackpropBatch            
# [31] validann::ann_SANN                   validann::ann_Nelder-Mead            h2o::h2o.deeplearning               
# [34] neuralnet::neuralnet_backprop        snnR::snnR                           simpleNeural::sN.MLPtrain           
# [37] AMORE::train_BATCHgd                 RSNNS::mlp_Quickprop                 AMORE::train_BATCHgdwm              
# [40] tensorflow::AdamOptimizer            tensorflow::MomentumOptimizer        tensorflow::GradientDescentOptimizer
# [43] neural::mlptrain                     neuralnet::neuralnet_sag             tensorflow::AdagradOptimizer        
# [46] tensorflow::FtrlOptimizer            tensorflow::AdadeltaOptimizer        softmaxreg::softmaxreg              
# [49] rcane::rlm     

scoredfr$pfa
# [1] "radiant.model::radiant.model"         "momlp::monmlp.fit_BFGS"               "nlsr::nlxb"                          
# [4] "minpack.lm::nlsLM"                    "rminer::fit"                          "validann::ann_BFGS"                  
# [7] "nnet::nnet"                           "neuralnet::neuralnet_slr"             "brnn::brnn_gaussNewton"              
#[10] "CaDENCE::cadence.fit_BFGS"            "qrnn::qrnn.fit"                       "validann::ann_L-BFGS-B"              
#[13] "validann::ann_CG"                     "ANN2::neuralnetwork_adam"             "RSNNS::mlp_SCG"                      
#[16] "neuralnet::neuralnet_rprop+"          "ANN2::neuralnetwork_rmsprop"          "ANN2::neuralnetwork_sgd"             
#[19] "RSNNS::mlp_Rprop"                     "AMORE::train_ADAPTgdwm"               "RSNNS::mlp_BackpropWeightDecay"      
#[22] "neuralnet::neuralnet_rprop-"          "RSNNS::mlp_Std_Backpropagation"       "AMORE::train_ADAPTgd"                
#[25] "RSNNS::mlp_BackpropChunk"             "RSNNS::mlp_BackpropMomentum"          "validann::ann_Nelder-Mead"           
#[28] "RSNNS::mlp_BackpropBatch"             "validann::ann_SANN"                   "deepnet::gradientdescent"            
#[31] "automl::automltrainmanual_trainwpso"  "momlp::monmlp.fit_Nelder-Mead"        "RSNNS::mlp_Quickprop"                
#[34] "h2o::h2o.deeplearning"                "elmNNRcpp::train_extremeML"           "AMORE::train_BATCHgd"                
#[37] "neuralnet::neuralnet_backprop"        "snnR::snnR"                           "automl::automltrainmanual_trainwgrad"
#[40] "AMORE::train_BATCHgdwm"               "keras::modelsequential_adadelta"      "neuralnet::neuralnet_sag"            
#[43] "keras::modelsequential_adagrad"       "ELMR::OSelmtrain.formula_extremeML"   "keras::modelsequential_sgd"          
#[46] "keras::modelsequential_adam"          "keras::modelsequential_rmsprop"   


## ==========
## SOME PLOTS
## ==========
plot(scoredfr[,c("duration.score", "RMSE.score", "med.score", "d51.score")], las = 1)


pdf("Rplots_RMSE_med_duration_d51.pdf", width = 13, height = 4.5, paper = "a4r")
	op <- par(mfrow = c(1,3), las = 1, mar = c(0,0.5,0,0.5), oma = c(2,2,3.5,2), cex = 1.1)
	plot(scoredfr[,c("RMSE.score", "med.score")]); abline(v=10.5, lty = 2)
	mtext("x=RMSE.score,  y=med.score", line = 1.5, font = 2)
	plot(scoredfr[,c("RMSE.score", "duration.score")], yaxt = "n"); abline(v=10.5, lty = 2)
	mtext("x=RMSE.score,  y=duration.score", line = 1.5, font = 2)
	plot(scoredfr[,c("RMSE.score", "d51.score")], yaxt = "n"); Axis(scoredfr[,5], side = 4)
	mtext("x=RMSE.score,  y=d51.score", line = 1.5, font = 2)
	# mtext("(x=RMSE.score, y=med.score)    (x=RMSE.score, y=duration.score)    (x=RMSE.score, y=d51.score)", 
		  # outer = TRUE, line = 2, font = 2)
	par(op)
dev.off()


## =====================================
## GLOBAL SCORE APPLIED TO EVERY DATASET
## =====================================
merge_sfr_dfr <- function(x, y) {
	z <- cbind(
			x[,c("npfa","pfa","duration.mean","RMSE.min","duration.rank","RMSE.rank")], 
			y[,c("duration.score","RMSE.score")]
		)
	z[order(z$RMSE.score),]
}
zfr <- lapply(sfr, merge_sfr_dfr, y = scoredfr0); zfr


## =========================
## GRAPHIC RMSEscore_RMSEmin
## =========================
pdf("Rplots_RMSEscore_RMSEmin.pdf")
op <- par(mfrow = c(3,4), las = 1, mar = c(0,0,0,0), oma = c(1,1,3,1))
for (j in seq_along(zfr)) {
names(zfr)[j]
plot(log1p(zfr[[j]][, "RMSE.score"]), log1p(zfr[[j]][, "RMSE.min"]),
     xlab = "RMSE.score", ylab = "RMSE.min", # main = names(zfr)[j], 
     las = 1, col = 0, xaxt = "n", yaxt = "n")
mtext(names(zfr)[j], line = -1.2, cex = 0.8)
text(log1p(zfr[[j]][, "RMSE.score"]), log1p(zfr[[j]][, "RMSE.min"]),
	 labels = zfr[[j]][, "RMSE.score"])
}
mtext("x=RMSE.score (global)   y=RMSE.min (per dataset)", outer = TRUE, line = 1)
dev.off()


## ==============================
## GRAPHIC RMSEscore_durationmean
## ==============================
pdf("Rplots_RMSEscore_durationmean.pdf")
op <- par(mfrow = c(3,4), las = 1, mar = c(0,0,0,0), oma = c(1,1,3,1))
for (j in seq_along(zfr)) {
names(zfr)[j]
plot(log1p(zfr[[j]][, "RMSE.score"]), log1p(zfr[[j]][, "duration.mean"]),
     xlab = "RMSE.score", ylab = "RMSE.min", # main = names(zfr)[j], 
     las = 1, col = 0, xaxt = "n", yaxt = "n")
mtext(names(zfr)[j], line = -1.2, cex = 0.8)
text(log1p(zfr[[j]][, "RMSE.score"]), log1p(zfr[[j]][, "duration.mean"]),
	 labels = zfr[[j]][, "RMSE.score"])
}
mtext("x=RMSE.score (global)   y=duration.mean (per dataset)", outer = TRUE, line = 1)
dev.off()


## =======================================
## GRAPHIC RMSEmin_durationmean - 49 algos
## =======================================
pdf("Rplots_RMSEmin_durationmean-49.pdf")
op <- par(mfrow = c(3,4), las = 1, mar = c(0,0,0,0), oma = c(1,1,3,1))
for (j in seq_along(zfr)) {
names(zfr)[j]
plot(log1p(zfr[[j]][, "RMSE.min"]), log1p(zfr[[j]][, "duration.mean"]),
     xlab = "RMSE.min", ylab = "duration.mean", # main = names(zfr)[j], 
     las = 1, col = 0, xaxt = "n", yaxt = "n")
mtext(names(zfr)[j], line = -1.2, cex = 0.8)
text(log1p(zfr[[j]][, "RMSE.min"]), log1p(zfr[[j]][, "duration.mean"]),
	 labels = zfr[[j]][, "RMSE.score"])
}
mtext("x=RMSE.min (per dataset)   y=duration.mean (per dataset)    49 algos", outer = TRUE, line = 1)
dev.off()


## =======================================
## GRAPHIC RMSEmin_durationmean - 12 algos
## =======================================
pdf("Rplots_RMSEmin_durationmean-12.pdf")
op <- par(mfrow = c(3,4), las = 1, mar = c(0,0,0,0), oma = c(1,1,3,1))
for (j in seq_along(zfr)) {
names(zfr)[j]
plot(log1p(zfr[[j]][1:12, "RMSE.min"]), log1p(zfr[[j]][1:12, "duration.mean"]),
     xlab = "RMSE.min", ylab = "duration.mean", # main = names(zfr)[j], 
     las = 1, col = 0, xaxt = "n", yaxt = "n")
mtext(names(zfr)[j], line = -1.2, cex = 0.8)
text(log1p(zfr[[j]][1:12, "RMSE.min"]), log1p(zfr[[j]][1:12, "duration.mean"]),
	 labels = zfr[[j]][1:12, "RMSE.score"])
}
mtext("x=RMSE.min (per dataset)   y=duration.mean (per dataset)    12 algos", outer = TRUE, line = 1)
dev.off()


## =======================================
## GRAPHIC RMSEmin_durationmean - 09 algos
## =======================================
pdf("Rplots_RMSEmin_durationmean-09.pdf")
op <- par(mfrow = c(3,4), las = 1, mar = c(0,0,0,0), oma = c(1,1,3,1))
for (j in seq_along(zfr)) {
names(zfr)[j]
plot(log1p(zfr[[j]][1:9, "RMSE.min"]), log1p(zfr[[j]][1:9, "duration.mean"]),
     xlab = "RMSE.min", ylab = "duration.mean", # main = names(zfr)[j], 
     las = 1, col = 0, xaxt = "n", yaxt = "n")
mtext(names(zfr)[j], line = -1.2, cex = 0.8)
text(log1p(zfr[[j]][1:9, "RMSE.min"]), log1p(zfr[[j]][1:9, "duration.mean"]),
	 labels = zfr[[j]][1:9, "RMSE.score"])
}
mtext("x=RMSE.min (per dataset)   y=duration.mean (per dataset)    9 algos", outer = TRUE, line = 1)
dev.off()


## ========
## COMMENTS
## ========
## We can recommand the first 10 or 12 algorithms.
## ALGOS 5, 11 and 12 come from validann::ann_BFGS, ann_CG, ann_L-BFGS-B 
## and are said to be slower than others algos.
## I rerun validann::ann_BFGS*.R example on my computer which is consistently 
## 20-25 % slower than Bila's computer!
## There might be some slow wrapping functions in validann, not in optim().


scoredfr[1:12, "pfa"]
 # [1] nlsr::nlxb                 minpacklm::nlsLM           radiantmodel::radiantmodel
 # [4] rminer::fit                validann::ann_BFGS         nnet::nnet                
 # [7] MachineShop::fit.NNetModel qrnn::qrnn.fit             momlp::monmlp.fit_BFGS    
# [10] brnn::brnn_gaussNewton     validann::ann_CG           validann::ann_L-BFGS-B    


#[1] "radiant.model::radiant.model" "momlp::monmlp.fit_BFGS"       "nlsr::nlxb"                   "minpack.lm::nlsLM"           
#[5] "rminer::fit"                  "validann::ann_BFGS"           "nnet::nnet"                   "neuralnet::neuralnet_slr"    
#[9] "brnn::brnn_gaussNewton"       "CaDENCE::cadence.fit_BFGS"    "qrnn::qrnn.fit"               "validann::ann_L-BFGS-B" 

## THE END
## THE END


        
 

