## code to prepare `cours_eau` dataset goes here

cours_eau <- sf::st_read(
  dsn = "C:/Users/laura.meudec/Documents/Laura/Projet/Data/eau/",
  layer = "COURS_D_EAU_IDF")

usethis::use_data_raw("cours_eau")

usethis::use_data(cours_eau, overwrite = TRUE, version = 3)
