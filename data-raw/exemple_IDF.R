## code to prepare `exemple_IDF` dataset goes here

setwd("C:/Users/laura.meudec/Documents/Laura/Projet/Data/")
load(file = "IDF_new_2007.RData")

exemple_IDF <- IDF_new[sample(nrow(IDF_new), 1000), ]

usethis::use_data(exemple_IDF, overwrite = TRUE)
