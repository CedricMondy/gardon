## code to prepare `ASPE_tables` dataset goes here

base::setwd("C:/Users/laura.meudec/Documents/Laura/Projet/Data/")
base::load(file = "tables_sauf_mei_2022_10_17_10_13_40.RData")

base::setwd("C:/Users/laura.meudec/Documents/Laura/Projet/Data/")
base::load(file = "IDF_package.RData")

# ===================
# reduce big tables
# lot_poissons
lot_poissons_test <- IDF_new %>%
  dplyr::select(lop_id) %>%
  dplyr::left_join(lot_poissons) %>%
  unique()

# operation
operation_test <- IDF_new %>%
  dplyr::select(ope_id) %>%
  dplyr::left_join(operation) %>%
  unique()

#point_prelevement
point_prelevement_test <- IDF_new %>%
  dplyr::select(pop_id) %>%
  dplyr::left_join(point_prelevement) %>%
  unique()

# ref_entite_hydrographique
entite_hydro_test <- IDF_new %>%
  dplyr::select(enh_code_sandre) %>%
  dplyr::left_join(ref_entite_hydrographique) %>%
  unique()

#station
station_test <- IDF_new %>%
  dplyr::select(sta_code_sandre) %>%
  dplyr::left_join(station) %>%
  unique()

# ===================

# Change names
lot_poissons = lot_poissons_test
operation = operation_test
point_prelevement = point_prelevement_test
ref_entite_hydrographique = entite_hydro_test
station = station_test

# ===================

# Save data
usethis::use_data(point_prelevement,
                  operation,
                  ref_unite_hydrographique,
                  station,
                  region,
                  departement,
                  commune,
                  ref_type_projection,
                  lot_poissons,
                  ref_type_lot,
                  ref_espece,
                  prelevement_elementaire,
                  groupe_points,
                  ambiance,
                  passage,
                  operation_donnees_environnementales,
                  facies,
                  operation_description_peche,
                  ref_entite_hydrographique,
                  overwrite = TRUE)
