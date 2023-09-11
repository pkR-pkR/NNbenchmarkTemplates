

setwd("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNbenchmarkTemplates/results_2020_gsoc2020")
f1 <- read.csv("mDette-result-summary.csv")
pkgtested201920 <- unique(sapply(strsplit(f1$pfa, split="::"), head, n=1))

library(RWsearch)
crandb_down()

#discarded packages in 2019-2020
pkgdisc <- read.csv("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNbenchmarkArticle3/NNbenchmarkArticle/input/tables/TableDiscardedPkg.csv")[, 2]

#living packages
pkgtested201920[pkgtested201920 %in% crandb$Package]

#archived packages
pkgtested201920[!pkgtested201920 %in% crandb$Package]

#new packages
allNN <- s_crandb("neural", "networks", mode = "and")
length(allNN)
allNN <- setdiff(allNN, pkgdisc)
length(allNN)
allNN <- setdiff(allNN, pkgtested201920)
length(allNN)
pkg2check <- setdiff(allNN, c("NNbenchmark"))

pkg2chek.dep <- p_deps(pkg2check)

#some depends on previously tested packages
sapply(pkg2chek.dep, function(x) x[x %in% pkgtested201920])
noneed2search <- sapply(sapply(pkg2chek.dep, function(x) x[x %in% pkgtested201920]), paste, collapse=";")
noneed2search <- noneed2search[noneed2search != ""]
cbind(noneed2search)


#some are new
pkg2chek.dep2 <- sapply(pkg2chek.dep, function(x) all(!x %in% pkgtested201920))
pkg2chek.dep2 <- names(pkg2chek.dep2[pkg2chek.dep2])

#further recusrive dependencies
pkg2chek.dep <- p_deps(pkg2chek.dep2, recursive = TRUE)
noneed2search2 <- sapply(sapply(pkg2chek.dep, function(x) x[x %in% pkgtested201920]), paste, collapse=";")
noneed2search2 <- noneed2search2[noneed2search2 != ""]
cbind(noneed2search2)

pkg2check2 <- setdiff(setdiff(pkg2check, names(noneed2search)), names(noneed2search2))

pkg2torch <- unlist(sapply(pkg2chek.dep[pkg2check2], function(x) "torch" %in% x))
pkg2torch <- names(pkg2torch[pkg2torch])
