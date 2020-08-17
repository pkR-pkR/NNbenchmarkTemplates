# =======
# R CHECK
# =======

#install.packages("rhub")
library("rhub")
rhub::check_for_cran(path = "D:/GSoC2020/NNbenchmark", email = "bila.mahdi@mhs.unsyiah.ac.id")
platforms <- rhub::platforms()
platforms$name
rhub::check_for_cran(path = "D:/GSoC2020/NNbenchmark", email = "bila.mahdi@mhs.unsyiah.ac.id", 
                     platforms = platforms$name)
