## code to prepare `IDF_contour` dataset goes here

IDF_contour <- sf::st_read(
  dsn = "C:/Users/laura.meudec/Documents/Laura/Projet/Data/IDF_masques/",
  layer = "MASQUE_IdF")

usethis::use_data(IDF_contour, overwrite = TRUE)
