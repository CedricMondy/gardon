## code to prepare `altitude_IDF` dataset goes here
setwd("C:/Users/laura.meudec/Documents/Laura/Projet/Data/")
altitude_IDF <- readr::read_csv("points_prelevements_avec_altitude.csv")

usethis::use_data(altitude_IDF, overwrite = TRUE)
