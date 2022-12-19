# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Creation d'une base de donnees regionale
#' @description Creation d'une base de donnees unique en joignant les tables extraites d'ASPE. Peut aussi transformer les coordonnees selon le referentiel desire.
#'
#' @param file.name Nom des donnees en format .RData correspondant a un extrait d'ASPE.
#' @param package.data Argument logique `TRUE/FALSE`. Indique si les donnees exemple du package doivent etre utilisees.
#' @param file.name.mei Nom des donnees en format .RData correspondant a la table des mesures individuelles.
#' @param station.filter Table additionnelle contenant le numero Sandre des stations a conserver.
#' @param colname.station.filter Colonne de `station.filter` qui contient le numero des stations Sandre.
#' @param type.ce.IDF Table additionnelle contenant les types de cours d'eau et qui est reliee aux autres tables par le code Sandre.
#' @param colname.type.ce.sta Colonne de `type.ce.IDF` qui contient le numero des stations Sandre.
#' @param dir Chemin d'acces ou trouver le fichier de `file.name`. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param region.code.insee Code Insee de la region d'interet. Doit etre accompagne de `echelle = "region"`. Par defaut `region.code.insee = NULL` et toutes les donnees seront conservees.
#' @param departement.code.insee Code Insee du departement d'interet. Doit etre accompagne de `echelle = "departement"`. Par defaut `departement.code.insee = NULL` et toutes les donnees seront conservees.
#' @param unh.code.sandre Code sandre de l'unite hydrographique d'interet. Doit etre accompagne de `echelle = "bassin"`. Par defaut `unh.code.sandre = NULL` et toutes les donnees seront conservees.
#' @param echelle Quatre possibilites. (1) Besoin de selectionner une ou plusieurs (`c()`) regions, alors `echelle = "region"`. (2) Besoin de selectionner un ou plusieurs (`c()`) departements, alors `echelle = "departement"`. (3) Besoin de selectionner une ou plusieurs (`c()`) unite hydrographique, alors `echelle = "bassin"`. (4) Besoin de toutes les donnees, alors `echelle = NULL`.
#' @param date Deux possibilites. (1) Date minimum a partir de laquelle on veut les donnees au format `'%Y-%m-%d'`. (2) Periode de temps pendant laquelle on veut les donnees au format `c('%Y-%m-%d', '%Y-%m-%d')`. Par defaut `date = NULL`.
#' @param CRS CRS souhaite. Celui de la France metropolitaine est 2154. Par defaut `CRS = NULL` et les coordonnees ne seront pas mofidiees.
#' @param correct.coord Argument logique `TRUE/FALSE`. Par defaut `correct.coord = FALSE` et les coordonnees ne sont pas modifiees.
#' @param save Argument logique `TRUE/FALSE`. Par defaut `save = FALSE` et la tableau de donnees cree n'est pas enregistre dans le repertoire de travail.
#' @param ecrevisse Argument logique `TRUE/FALSE`. Par defaut `save = FALSE` et les ecrevisses sont gardees dans la base de donnees.
#' @return La nouvelle base de donnees au format dataframe.
#' @export
#'
#' @examples
#' test_df <- getDataRegion(
#' file.name = NULL,
#' package.data = TRUE, #utilisation des donnees en exemple
#' file.name.mei = NULL,
#' station.filter = NULL,
#' region.code.insee = 11, #la region ile de france
#' departement.code.insee = NULL,
#' echelle = "region",
#' date = '2007-01-01', #donnees a partir de 2007
#' CRS = 2154, #le crs de la France metropolitaine
#' correct.coord = TRUE,
#' save = FALSE,
#' ecrevisse = FALSE
#' )
#'
#' @importFrom dplyr right_join left_join inner_join filter full_join group_by group_split mutate select
#' @importFrom purrr map_df
#' @importFrom readr read_delim
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom tidyr separate
getDataRegion <- function(file.name = NULL,
                          package.data = TRUE,
                          file.name.mei = NULL,
                          station.filter = NULL,
                          colname.station.filter = NULL,
                          type.ce.IDF = NULL,
                          colname.type.ce.sta = NULL,
                          dir = getwd(),
                          region.code.insee = NULL,
                          departement.code.insee = NULL,
                          unh.code.sandre = NULL,
                          echelle = NULL,
                          date = NULL,
                          CRS = NULL,
                          correct.coord = FALSE,
                          save = FALSE,
                          ecrevisse = FALSE) {

  # =====================

  # Load data
  if (package.data == TRUE) {
    print("chargement des donnees du package")
  } else if (package.data == FALSE) {
    if(is.null(file.name) == FALSE) {
      base::setwd(paste0(dir, "/"))
      df <- base::load(file = file.name)
    } else if (is.null(file.name) == TRUE) {
      stop("file.name doit etre renseigne")
    }
  }


  # Load data "mesure individuelle"
  if(is.null(file.name.mei) == FALSE) {
    base::setwd(paste0(dir, "/"))
    df1 <- base::load(file = file.name.mei)
  } else if (is.null(file.name.mei) == TRUE) {
    print("table de mesure individuelle non renseignee")
  }

  # Present station sampled
  if (is.null(station.filter) == FALSE & package.data == FALSE &
      is.null(colname.station.filter) == FALSE) {
    base::setwd(paste0(dir, "/"))
    PELEC_pp_liste <- readr::read_delim(station.filter,
                                        delim = "\t", escape_double = FALSE,
                                        trim_ws = TRUE)
  } else if (is.null(station.filter) == FALSE & package.data == FALSE &
             is.null(colname.station.filter) == TRUE) {
    stop("station.filter est renseigne alors colname.station.filter
                    doit aussi etre renseigne")
  } else if (is.null(station.filter) == TRUE & package.data == TRUE) {
    print("pas de donnees d'exemple de station.filter dans package")
  } else if (is.null(station.filter) == FALSE & package.data == TRUE) {
    stop("impossible que station.filter soit renseigne et
             package.data = TRUE")
  }

  # Data type cours d'eau
  if (is.null(type.ce.IDF) == FALSE & package.data == FALSE &
      is.null(colname.type.ce.sta) == FALSE) {
    base::setwd(paste0(dir, "/"))
    TypeCE_RCS_IDF <- readr::read_delim(type.ce.IDF,
                                        delim = "\t", escape_double = FALSE,
                                        trim_ws = TRUE)
  } else if (is.null(type.ce.IDF) == FALSE & package.data == FALSE &
             is.null(colname.type.ce.sta) == TRUE) {
    stop("type.ce.IDF est renseigne alors colname.type.ce.sta
           doit aussi etre renseigne")
  } else if (is.null(type.ce.IDF) == TRUE & package.data == TRUE) {
    print("pas de donnees d'exemple de type.ce dans package")
  } else if (is.null(type.ce.IDF) == FALSE & package.data == TRUE) {
    stop("impossible que type.ce.IDF soit renseigne et
             package.data = TRUE")
  }
  # =====================

  # Join data of "operation" + "point_prelevement" + "ref_unite_hydrographique"
  # + "station"
  join_data <- point_prelevement %>%
    dplyr::right_join(operation, by = c("pop_id" = "ope_pop_id")) %>%
    dplyr::left_join(ref_unite_hydrographique,
                     by = c("pop_unh_id" = "unh_id")) %>%
    dplyr::left_join(station, by = c("pop_sta_id" = "sta_id"))

  # Join with location (INSEE number) and filter for region of interest
  location <- region %>%
    dplyr::inner_join(departement,
                      by = c("reg_code_insee" = "dep_reg_code_insee")) %>%
    dplyr::inner_join(commune, by = c("dep_code_insee" = "com_dep_code_insee"))

  # Separate "points sandre" and "points wama"
  wama_only <- join_data %>%
    dplyr::filter(is.na(pop_sta_id) == TRUE)

  sandre_only <- join_data %>%
    dplyr::filter(is.na(pop_sta_id) == FALSE)

  # Join with location information
  wama_loc <- dplyr::left_join(wama_only, location,
                               by = c("pop_com_code_insee_wama" = "com_code_insee"))
  sandre_loc <- dplyr::left_join(sandre_only, location,
                                 by = c("sta_com_code_insee" = "com_code_insee"))

  # Join two previous tables and filter to get region of interest
  IDF_loc <- dplyr::full_join(wama_loc, sandre_loc, by = NULL) %>%
    dplyr::left_join(ref_type_projection, by = c("pop_typ_id" = "typ_id"))
  if (is.null(echelle) == TRUE) {
    print("pas de region ou de departement ou de bassin hydrographique
          renseigne : donnees france entiere")
    # par region
  } else if (echelle == "region") {
    if (is.null(region.code.insee) == FALSE) {
      IDF_loc <- IDF_loc %>%
        dplyr::filter(reg_code_insee %in% region.code.insee)
    } else if (is.null(region.code.insee) == TRUE) {
      stop("un numero de region doit etre renseigne")
    }
    # par departement
  } else if (echelle == "departement") {
    if (is.null(departement.code.insee) == FALSE) {
      IDF_loc <- IDF_loc %>%
        dplyr::filter(dep_code_insee %in% departement.code.insee)
    } else if (is.null(departement.code.insee) == TRUE) {
      stop("un numero de departement doit etre renseigne")
    }
    # par bassin hydrographique
  } else if (echelle == "bassin") {
    if (is.null(unh.code.sandre) == FALSE) {
      IDF_loc <- IDF_loc %>%
        dplyr::filter(unh_code_sandre %in% unh.code.sandre)
    } else if (is.null(unh.code.sandre) == TRUE) {
      stop("un code d'unite hydrographique doit etre renseigne")
    }
  }

  # =====================

  # Correct coordinates
  new_coord <- IDF_loc
  if (correct.coord == TRUE) {
    new_coord <- new_coord %>%
      dplyr::group_by(typ_code_epsg) %>%
      dplyr::group_split() %>%
      purrr::map_df(
        .f = function(df1) {
          old_crs = unique(df1$typ_code_epsg)

          df1 %>%
            sf::st_as_sf(coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
                         crs = old_crs, remove = T) %>%
            sf::st_transform(crs = CRS) %>%
            dplyr::mutate(pop_coordonnees_x = sf::st_coordinates(.)[,1],
                          pop_coordonnees_y = sf::st_coordinates(.)[,2])
        }
      )
  } else if (correct.coord == FALSE) {
    print("pas de correction des coordonnees")
  }


  # =====================

  # Joindre les infos de lots et d'espèces
  lots_espece <- lot_poissons %>%
    dplyr::left_join(ref_type_lot, by = c("lop_tyl_id" = "tyl_id")) %>%
    dplyr::left_join(ref_espece, by = c("lop_esp_id" = "esp_id")) %>%
    dplyr::select(lop_id : lop_tlo_id, tyl_code_sandre, esp_code_sandre,
                  esp_code_alternatif, esp_nom_commun, esp_nom_latin)

  # Joindre les donnes "prelevement elementaire" avec "groupe_point" et
  #"ambiance" et "passage" + info espèces
  pre_point <- dplyr::left_join(prelevement_elementaire, groupe_points,
                                by = c("pre_id" = "grp_id")) %>%
    dplyr::left_join(ambiance, by = c("pre_id" = "amb_id")) %>%
    dplyr::left_join(passage, by = c("pre_id" = "pas_id")) %>%
    dplyr::left_join(lots_espece, by = c("pre_id" = "lop_pre_id"))

  # =====================

  # Joindre avec les donnees d'IDF recente et parametre IPR
  IDF_new <- new_coord %>%
    # separate date column to have "ope_jour" and "ope_heure"
    tidyr::separate(col = ope_date, into = c("ope_day", "ope_heure"),
                    sep = " ") %>%
    # Convert to data format
    dplyr::mutate(ope_jour = as.Date(ope_day, format = "%Y-%m-%d"))
  # filtrer pour avoir data apres 2007
  if (length(date) == 1) {
    IDF_new <- IDF_new %>%
      dplyr::filter(ope_jour >= date)
  } else if (length(date) == 2) {
    date_start <- date[1]
    date_fin <- date[2]
    IDF_new <- IDF_new %>%
      dplyr::filter(ope_jour >= date_start & ope_jour <= date_fin)
  } else if (is.null(date) == TRUE) {
    print("Pas de filtre sur les dates")
  }
  # avec data "operation_donnees_environnementales"
  IDF_new <- IDF_new %>%
    dplyr::inner_join(operation_donnees_environnementales,
                      by = c("ope_id" = "ode_ope_id")) %>%
    # donnees facies
    dplyr::left_join(facies,
                     by = c("ope_id" = "fac_ode_ope_id")) %>%
    # avec data "operation_description_peche"
    dplyr::inner_join(operation_description_peche,
                      by = c("ope_id" = "odp_ope_id"))
  # avec data recente
  if(is.null(station.filter) == FALSE) {
    IDF_new <- IDF_new %>%
      dplyr::inner_join(PELEC_pp_liste,
                        by = c("sta_code_sandre" =
                                 colname.station.filter))
  } # if we only want the present station
  # Pour avoir les donnees de type de cours d'eau
  if(is.null(type.ce.IDF) == FALSE) {

    TypeCE_RCS_IDF <- TypeCE_RCS_IDF %>%
      transform(colname.type.ce.sta =
                  as.character(colname.type.ce.sta))
    IDF_new <- IDF_new %>%
      dplyr::left_join(., TypeCE_RCS_IDF,
                       by = c("sta_code_sandre" =
                                colname.type.ce.sta))
    typ_eau_var <- c("Type_cours_eau")
  } else if (is.null(type.ce.IDF) == TRUE) {
    typ_eau_var <- 0
  }
  # entite hydro
  IDF_new <- IDF_new %>%
    dplyr::left_join(ref_entite_hydrographique,
                     by = c("sta_enh_id" = "enh_id")) %>%
    # avec pre_point
    dplyr::left_join(pre_point, by = c("ope_id" = "pre_ope_id"))

  # Avec mesure individuelle
  if (is.null(file.name.mei) == FALSE) {
    IDF_new <- IDF_new %>%
      dplyr::left_join(mesure_individuelle, by = c("lop_id" = "mei_lop_id"))
    mei_var <- c("mei_id", "mei_taille", "mei_poids", "mei_poids_estime")
  } else if (is.null(file.name.mei) == TRUE) {
    mei_var <- 0
  }
  # Filtrer ou pas les ecrevisse
  if (ecrevisse == FALSE) {
    IDF_new <- IDF_new %>%
      filter(esp_code_alternatif != "OCL" & esp_code_alternatif != "PCC" &
               esp_code_alternatif != "PFL")
  } else if (ecrevisse == TRUE) {
    print("Presence potentielle ecrevisses dans donnees")
  }
  # select data
  IDF_new <- IDF_new %>%
    dplyr::select(sta_code_sandre, sta_com_code_insee, enh_code_sandre,
                  enh_libelle_sandre, sta_eligibilite_ipr_iprplus,
                  reg_code_insee, all_of(typ_eau_var), # "station"
                  # "operation"
                  ope_id, ope_code_wama, ope_id_wama, ope_jour, ope_pro_id,
                  ope_sans_poisson,
                  # "point_prelevement"
                  pop_id, pop_code_sandre, pop_code_wama, pop_sta_id,
                  pop_com_code_insee_wama,
                  # Hydrography
                  unh_code_sandre,
                  # Coordinates
                  pop_coordonnees_x, pop_coordonnees_y,
                  # Facies
                  fac_profondeur_moyenne, fac_importance_relative,
                  fac_tyf_id,
                  grp_nombre_points_facies_profond,
                  grp_nombre_points_facies_courant,
                  grp_nombre_points_facies_plat,
                  grp_nombre_points_annexe,
                  grp_nombre_points_chenal,
                  grp_nombre_points_berge,
                  # mesure individuelle
                  all_of(mei_var),
                  # prelevement elementaire
                  pre_tpe_id, pre_id, grp_tgp_id, grp_nombre, #amb_longueur,
                  # amb_largeur, amb_profondeur, odp_largeur_rive_gauche,
                  # odp_largeur_rive_droite, odp_longueur_rive_gauche,
                  # odp_longueur_rive_droite, amb_code_zone, pas_numero,
                  # ref protocole
                  ope_pro_id,
                  # especes et lots
                  lop_id : lop_tlo_id, tyl_code_sandre, esp_code_sandre,
                  esp_code_alternatif, esp_nom_commun, esp_nom_latin,
                  # data IPR_env
                  pop_altitude, pop_surface_bassin_versant_amont,
                  pop_distance_source, pop_temperature_moyenne_janvier,
                  pop_temperature_moyenne_juillet, ope_surface_calculee,
                  ode_profondeur_moyenne_station, pop_pente_ign_cours_eau,
                  odp_largeur_lame_eau, odp_longueur) %>%
    as.data.frame()

  # =====================
  if (save == TRUE) {
    base::setwd("C:/Users/laura.meudec/Documents/Laura/Projet/Data/")
    save(IDF_new, file = "IDF_package.RData")
  }
  return(IDF_new)
}


# ===========================

#' Creation de tableaux au sein des fonctions
#' @description Permet de creer des tableaux en modifiant la taille de la police, les couleurs, le titre, le commentaire.
#'
#' @param x Data au format dataframe.
#' @param colours Couleurs pour chaque cellules du tableau en partant de la gauche.
#' @param titre Titre du tableau entre `""`.
#' @param comment Character entre `""`. Commentaire a ajouter en dessous du tableau.
#' @param size.tab Numerique. Taille des caracteres du tableau.
#' @param size.titre Numerique. Taille du titre.
#' @param comment.size Numerique. Taille du commentaire.
#' @return Un tableau contenant les statistiques de la fonction.
#' @export
#'
#' @examples
#' table_stat <- make_tab(x = tempJanv_stat,
#'                        colours = c("#FF6BA1", "#FF6BA1",
#'                                    "#FF6BA1", "#73FF7D"),
#'                        titre = "Rapport Temperature janvier (%)",
#'                        comment = commentaire)
#'
#' @importFrom grid textGrob gpar unit grobHeight
#' @importFrom gridExtra ttheme_default tableGrob
#' @importFrom gtable gtable_add_rows gtable_add_grob
make_tab <- function(x,
                     colours = NULL,
                     titre,
                     comment,
                     size.tab = 10,
                     size.titre = 12,
                     comment.size = 9) {

  title <- grid::textGrob(titre, gp = grid::gpar(fontsize = size.titre))
  tt <- gridExtra::ttheme_default(core = base::list(bg_params = base::list(
    fill = colours, col = "gray56")), base_size = size.tab)
  footnote <- grid::textGrob(comment, x = 0, hjust = 0,
                             gp = grid::gpar(fontface = "italic",
                                             fontsize = comment.size))
  table <- gridExtra::tableGrob(x, theme = tt)
  padding <- unit(2, "line")
  table <- gtable::gtable_add_rows(table,
                                   heights = grid::grobHeight(title)
                                   + padding, pos = 0)
  table <- gtable::gtable_add_rows(table,
                                   heights = grid::grobHeight(footnote) +
                                     padding)
  table <- gtable::gtable_add_grob(table, base::list(title, footnote),
                                   t = c(1, base::nrow(table)),
                                   l = c(1, 2),
                                   r = base::ncol(table))
  base::return(table)
}
