## ----message=FALSE, warning=FALSE------------------------------------------------
library(NNbenchmark)
library(kableExtra)
library(dplyr)   
library(stringr) 
options(scipen = 999)

odir <- "~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNbenchmarkTemplates/results_2020_gsoc2020"
  


## --------------------------------------------------------------------------------
#lf   <- lapply(list.files(odir, pattern = "-results.csv", full.names = TRUE), csv::as.csv)
#names(lf) <- names(NNbigdatasets)
lf <- csv::as.csv(paste0(odir, "/bWoodN1-results.csv"))
head(lf)
colnames(lf) <- c("event", "RMSE", "MSE", "MAE", "WAE", "time")

glf <- cbind(ds   = str_remove(str_extract(lf$event, "\\w+_"), "_"),
					  pfa  = str_sub(str_remove(lf$event, str_extract(lf$event, "\\w+_")),  1, -4),
					  run  = str_sub(lf$event, -2, -1),
					  lf[,c("RMSE","MAE","WAE","time")])
head(glf)
                       
summary.glf <- cbind(                       
  time.med = tapply(glf$time, glf$pfa, median) ,
  time.mean = tapply(glf$time, glf$pfa, mean) ,
  time.sd = tapply(glf$time, glf$pfa, sd) ,
  RMSE.min = tapply(glf$RMSE, glf$pfa, min) ,
  RMSE.med = tapply(glf$RMSE, glf$pfa, median) ,
  RMSE.mean = tapply(glf$RMSE, glf$pfa, mean) ,
  RMSE.d51 = tapply(glf$RMSE, glf$pfa, function(x) median(x) - min(x)) ,
  MAE.mean = tapply(glf$MAE, glf$pfa, mean) ,
  MAE.med = tapply(glf$MAE, glf$pfa, median) ,
  WAE.mean = tapply(glf$WAE, glf$pfa, mean) ,
  WAE.med = tapply(glf$WAE, glf$pfa, median) )





## --------------------------------------------------------------------------------

scoredfr0 <- as.data.frame(summary.glf[, c("RMSE.min", 
					    "RMSE.med", "RMSE.d51", "MAE.med", "WAE.med", "time.med")])

scoredfr <- scoredfr0[order(scoredfr0$RMSE.min),]
write.csv(scoredfr0, paste0(odir, "/bWoodN1-results-summary.csv"))

kable(scoredfr)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

