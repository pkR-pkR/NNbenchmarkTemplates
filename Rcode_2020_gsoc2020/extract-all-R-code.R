# extract R code 

library(knitr)

setwd("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNbenchmarkTemplates/Rmd_2020/")
rmdfile <- list.files(pattern=".Rmd")


sapply(rmdfile, purl)
