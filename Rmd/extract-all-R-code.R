# extract R code 

library(knitr)

setwd("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNbenchmarkTemplates/Rmd")
rmdfile <- list.files(pattern=".Rmd")


sapply(rmdfile, purl)
