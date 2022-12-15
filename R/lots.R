# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Conformite des lots
#' @description Verifie que les caracteristiques de chaque type lot sont conforme (effectif, poids, taille)
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_lotTyl"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param color.point Vecteur. Palette contenant les couleurs pour les points de la carte. Par defaut, `color.point = colorblind_8`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @return Des resultats graphiques : carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_lotTyl <- test_ope_lotTyl(
#' data = exemple_IDF,
#' csv = FALSE,
#' pdf = FALSE
#' )
#' @importFrom dplyr select mutate case_when inner_join group_by filter
#' @importFrom ggplot2 ggplot geom_sf geom_point scale_color_manual ggtitle labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_ope_lotTyl <- function(data,
                            stat = TRUE,
                            dir = getwd(),
                            file.name = "ope_lotTyl",
                            map = TRUE,
                            csv = TRUE,
                            color.point = colorblind_8,
                            map.contour = IDF_contour,
                            map.cours.eau = cours_eau,
                            pdf = TRUE) {
  # create data : ope + lot
  ope_lot <- data %>%
    dplyr::select(lop_id, ope_id)

  # create alerte
  data_lotTyl <- data %>%
    dplyr::select(lop_tyl_id,
                  lop_id,
                  lop_effectif,
                  lop_longueur_specimens_taille_mini,
                  lop_longueur_specimens_taille_maxi,
                  lop_poids) %>%
    unique() %>%
    dplyr::mutate(
      lotTyl_lotmessage = dplyr::case_when(
        is.na(lop_tyl_id) == TRUE ~ "Lot non renseigne",
        # LOT I
        lop_tyl_id == 1 & lop_effectif > 1 ~ "Lot I conforme",
        lop_tyl_id == 1 & lop_effectif <= 1 ~ "Lot I non conforme",
        lop_tyl_id == 1 & lop_poids > 0 ~ "Lot I conforme",
        lop_tyl_id == 1 & lop_poids <= 0 ~ "Lot I non conforme",
        #LOT S/L
        lop_tyl_id == 2 & lop_effectif > 30 ~ "Lot SL conforme",
        lop_tyl_id == 2 & lop_effectif <= 30 ~ "Lot SL non conforme",
        # LOT G
        lop_tyl_id == 3 & (
          is.na(lop_longueur_specimens_taille_mini) == F &
            is.na(lop_longueur_specimens_taille_maxi) == F) ~
          "Lot G conforme",
        lop_tyl_id == 3 & (
          is.na(lop_longueur_specimens_taille_mini) == T |
            is.na(lop_longueur_specimens_taille_maxi) == T) ~
          "Lot G non conforme",
        lop_tyl_id == 3 & (lop_longueur_specimens_taille_mini <
                             lop_longueur_specimens_taille_maxi) ~
          "Lot G conforme",
        lop_tyl_id == 3 & (lop_longueur_specimens_taille_mini >=
                             lop_longueur_specimens_taille_maxi) ~
          "Lot G non conforme",
        lop_tyl_id == 3 & lop_effectif > 1 ~ "Lot G conforme",
        lop_tyl_id == 3 & lop_effectif <= 1 ~ "Lot G non conforme",
        lop_tyl_id == 3 & lop_poids > 0 ~ "Lot G conforme",
        lop_tyl_id == 3 & lop_poids <= 0 ~ "Lot G non conforme",
        # LOT N
        lop_tyl_id == 4 & lop_effectif == 1 ~ "Lot N conforme",
        lop_tyl_id == 4 & lop_effectif != 1 ~ "Lot N non conforme"
      ),

      lotTyl_lottype = dplyr::case_when(
        is.na(lop_tyl_id) == TRUE ~ "alerte",
        # LOT I
        lop_tyl_id == 1 & lop_effectif > 1 ~ "",
        lop_tyl_id == 1 & lop_effectif <= 1 ~ "alerte",
        lop_tyl_id == 1 & lop_poids > 0 ~ "",
        lop_tyl_id == 1 & lop_poids <= 0 ~ "alerte",
        #LOT S/L
        lop_tyl_id == 2 & lop_effectif > 30 ~ "",
        lop_tyl_id == 2 & lop_effectif <= 30 ~ "alerte",
        # LOT G
        lop_tyl_id == 3 & (
          is.na(lop_longueur_specimens_taille_mini) == F &
            is.na(lop_longueur_specimens_taille_maxi) == F) ~ "",
        lop_tyl_id == 3 & (
          is.na(lop_longueur_specimens_taille_mini) == T |
            is.na(lop_longueur_specimens_taille_maxi) == T) ~ "alerte",
        lop_tyl_id == 3 & (lop_longueur_specimens_taille_mini <
                             lop_longueur_specimens_taille_maxi) ~ "",
        lop_tyl_id == 3 & (lop_longueur_specimens_taille_mini >=
                             lop_longueur_specimens_taille_maxi) ~ "alerte",
        lop_tyl_id == 3 & lop_effectif > 1 ~ "",
        lop_tyl_id == 3 & lop_effectif <= 1 ~ "alerte",
        lop_tyl_id == 3 & lop_poids > 0 ~ "",
        lop_tyl_id == 3 & lop_poids <= 0 ~ "alerte",
        # LOT N
        lop_tyl_id == 4 & lop_effectif == 1 ~ "",
        lop_tyl_id == 4 & lop_effectif != 1 ~ "alerte"
      )
    )

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_lotTyl) %>%
      dplyr::group_by(lop_id) %>%
      dplyr::filter(lotTyl_lottype == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = IDF_contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = cours_eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = as.factor(lop_tyl_id)),
                          size = 2) +
      ggplot2::scale_color_manual(values = color.point) +
      ggplot2::ggtitle("Carte des lots classees alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Type de lots")
  }

  # Create statistics
  if (stat == TRUE) {
    lotTyl_stat <- data_lotTyl %>%
      plyr::summarise(
        na = round(
          mean(lotTyl_lotmessage == "Lot non renseigne", na.rm = T) * 100,
          digits = 2),
        alerte = round(
          mean(lotTyl_lottype == "alerte", na.rm = T) * 100, digits = 2),
        lotG.normal = round(
          mean(lotTyl_lotmessage == "Lot G conforme", na.rm = T) * 100,
          digits = 2),
        lotG.anormal = round(
          mean(lotTyl_lotmessage == "Lot G non conforme", na.rm = T) * 100,
          digits = 2),
        lotN.normal = round(
          mean(lotTyl_lotmessage == "Lot N conforme", na.rm = T) * 100,
          digits = 2),
        lotN.anormal = round(
          mean(lotTyl_lotmessage == "Lot N non conforme", na.rm = T) * 100,
          digits = 2),
        lotI.normal = round(
          mean(lotTyl_lotmessage == "Lot I conforme", na.rm = T) * 100,
          digits = 2),
        lotI.anormal = round(
          mean(lotTyl_lotmessage == "Lot I non conforme", na.rm = T) * 100,
          digits = 2),
        lotSL.anormal = round(
          mean(lotTyl_lotmessage == "Lot SL non conforme", na.rm = T) * 100,
          digits = 2),
        lotSL.anormal = round(
          mean(lotTyl_lotmessage == "Lot SL non conforme", na.rm = T) * 100,
          digits = 2))

    lotTyl_nb <- data_lotTyl %>%
      plyr::summarise(
        na = sum(lotTyl_lotmessage == "Lot non renseigne", na.rm = T),
        alerte = sum(lotTyl_lottype == "alerte", na.rm = T),
        lotG.normal = sum(lotTyl_lotmessage == "Lot G conforme", na.rm = T),
        lotG.anormal = sum(lotTyl_lotmessage == "Lot G non conforme", na.rm = T),
        lotN.normal = sum(lotTyl_lotmessage == "Lot N conforme", na.rm = T),
        lotN.anormal = sum(lotTyl_lotmessage == "Lot N non conforme", na.rm = T),
        lotI.normal = sum(lotTyl_lotmessage == "Lot I conforme", na.rm = T),
        lotI.anormal = sum(lotTyl_lotmessage == "Lot I non conforme", na.rm = T),
        lotSL.normal = sum(lotTyl_lotmessage == "Lot SL conforme", na.rm = T),
        lotSL.anormal = sum(lotTyl_lotmessage == "Lot SL non conforme", na.rm = T))

    # Create tables
    commentaire <- c("Alertes lots non conformes")

    table_stat <- make_tab(x = lotTyl_stat,
                           colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                       "#BF3111", "#5AB5BF", "#BF3111",
                                       "#5AB5BF", "#BF3111", "#5AB5BF",
                                       "#BF3111"),
                           titre = "Rapport conformité lots (%)",
                           comment = commentaire, size.tab = 7)
    table_nb <- make_tab(x = lotTyl_nb,
                         colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                     "#BF3111", "#5AB5BF", "#BF3111",
                                     "#5AB5BF", "#BF3111", "#5AB5BF",
                                     "#BF3111"),
                         titre = "Rapport conformité lots (nb)",
                         comment = commentaire, size.tab = 7)
  }

  data_lotTyl <- data_lotTyl %>%
    dplyr::left_join(ope_lot, .)

  data_lotTyl <- data_lotTyl %>%
    dplyr::group_by(ope_id) %>%
    dplyr::group_split() %>%
    purrr::map_df(
      .f = function(df) {
        df %>%
          dplyr::mutate(
            lotTyl_type = dplyr::case_when(
              any(stringr::str_detect(lotTyl_lottype, "alerte")) == TRUE ~
                "alerte",
              any(stringr::str_detect(lotTyl_lottype, "alerte")) == FALSE ~
                ""
            )
          )
      }
    )

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (map == TRUE) {
      print(g1) # map
    }
    if (stat == TRUE) {
      grid::grid.newpage()
      grid::grid.draw(table_stat) # percent
      grid::grid.newpage()
      grid::grid.draw(table_nb) # nb
    }
    if (stat == FALSE & map == FALSE) {
      grid::grid.newpage()
    }
    grDevices::dev.off()
  }

  # Plot statistics
  if (stat == TRUE) {
    grid::grid.draw(table_stat) # percent
    grid::grid.newpage()
    grid::grid.draw(table_nb) # nb
  } else {
    print("no statistic draw ==> write stat = TRUE to see it")
  }

  # Plot the map
  if (map == TRUE) {
    print(g1)
  } else {
    print("no map draw ==> write map = TRUE to see it")
  }
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_lotTyl, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_lotTyl)
}

# ===========================

#' Conformite des effectifs par espece
#' @description Identifie les effectifs aberrants pour chaque espece a l'aide de l'IQR criterion.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_lotTyl"`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs d'effectif est trace.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param percent Numerique. Pourcentage de difference au maximum a partir duquel on identifie les outliers.
#' @param color.alerte Vecteur. Palette contenant les deux couleurs des alertes. Par defaut, `color.point = pal_alerte`.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param echelle Deux valeurs possibles. Si `echelle = espece` alors les effectifs sont evalues par espece sans distinction de station. Si `echelle = POP` alors les effectifs sont evalues par espece et par point de prelevement.

#' @return Des resultats graphiques : boxplot (`plot = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @examples
#' result_sp_effectif <- test_sp_effectif(
#' data = exemple_IDF,
#' csv = FALSE,
#' pdf = FALSE,
#' percent = 100,
#' method = 2)

#' @importFrom dplyr select group_by summarise left_join mutate case_when
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_boxplot geom_point theme coord_flip scale_color_manual labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_sp_effectif <- function(data,
                             stat = TRUE,
                             dir = getwd(),
                             file.name = "sp_effectif",
                             plot = TRUE,
                             map = TRUE,
                             save = FALSE,
                             csv = TRUE,
                             folder = NULL,
                             color.alerte = pal_alerte,
                             pdf = TRUE,
                             map.contour = IDF_contour,
                             map.cours.eau = cours_eau,
                             echelle = "espece") {

  # Create a new folder
  if (save == TRUE) {
    if (is.null(folder) == TRUE) {
      newfolder <- "test_sp_effectif"
      dir.create(file.path(dirname(dir), newfolder))
    } else {
      newfolder <- folder
    }
  }

  # ===================
  # Effectif de chaque espece par point de prelevement

  if(echelle == "POP") {
    data_distri <- data %>%
      dplyr::select(pop_id,
                    ope_id,
                    esp_code_alternatif,
                    lop_id,
                    lop_effectif,
                    esp_nom_commun,
                    ope_surface_calculee,
                    pop_coordonnees_x,
                    pop_coordonnees_y) %>%
      unique() %>%
      dplyr::group_by(ope_id,
                      esp_code_alternatif,
                      esp_nom_commun,
                      pop_id,
                      pop_coordonnees_x,
                      pop_coordonnees_y) %>%
      dplyr::summarise(sum_effectif = sum(lop_effectif))

    data_pop_med <- data_distri %>%
      dplyr::group_by(pop_id,
                      esp_code_alternatif) %>%
      dplyr::summarise(median_pop = round(median(sum_effectif, na.rm = T),
                                          digits = 0))

    data_distri <- left_join(data_distri, data_pop_med)

    # Create alerte effectif
    data_effectif <- data_distri %>%
      dplyr::filter(is.na(esp_code_alternatif) == FALSE) %>%
      dplyr::group_by(esp_code_alternatif, pop_id) %>%
      dplyr::group_split() %>%
      purrr::map_df(
        .f = function(df) {
          # Calcul des outliers
          min_value_eff <- quantile(
            df$sum_effectif, probs = 0.25, na.rm = TRUE) -
            1.5*IQR(df$sum_effectif, na.rm = TRUE)

          max_value_eff <- quantile(
            df$sum_effectif, probs = 0.75, na.rm = TRUE) +
            1.5*IQR(df$sum_effectif, na.rm = TRUE)

          df <- df %>%
            dplyr::mutate(
              effectif_spmessage = dplyr::case_when(
                is.na(sum_effectif) == TRUE ~ "effectif manquant",
                is.na(esp_code_alternatif) == TRUE ~ "espece manquante",
                sum_effectif < min_value_eff ~ "effectif trop faible",
                sum_effectif > max_value_eff ~ "effectif trop eleve",
                sum_effectif <= max_value_eff &
                  sum_effectif >= min_value_eff ~ "effectif normal"
              ),
              effectif_sptype = dplyr::case_when(
                is.na(sum_effectif) == TRUE ~ "alerte",
                is.na(esp_code_alternatif) == TRUE ~ "alerte",
                sum_effectif < min_value_eff ~ "alerte",
                sum_effectif > max_value_eff ~ "alerte",
                sum_effectif <= max_value_eff &
                  sum_effectif >= min_value_eff ~ ""
              ),
              ecart_speffectif = dplyr::case_when(
                is.na(sum_effectif) == TRUE ~ NA_real_,
                is.na(esp_code_alternatif) == TRUE ~ NA_real_,
                sum_effectif < min_value_eff ~ min_value_eff - sum_effectif,
                sum_effectif > max_value_eff ~ sum_effectif - max_value_eff,
                sum_effectif <= max_value_eff &
                  sum_effectif >= min_value_eff ~ 0
              )
            )
        }
      )

    # Create boxplot and map with alertes of effectif
    plot_distri <- data_effectif %>%
      dplyr::filter(is.na(esp_code_alternatif) == FALSE) %>%
      dplyr::group_by(esp_code_alternatif) %>%
      dplyr::group_split() %>%
      purrr::map(
        .f = function(df) {
          # Create boxplot
          if(plot == TRUE) {
            g1 <- ggplot2::ggplot(
              aes(x = forcats::fct_reorder(as.factor(pop_id), median_pop),
                  y = sum_effectif), data = df) +
              ggplot2::geom_boxplot() +
              ggplot2::geom_point(aes(color = effectif_sptype)) +
              ggplot2::scale_color_manual(values = color.alerte) +
              ggplot2::labs(x = "Point de prélèvement", y = "Effectif") +
              ggplot2::ggtitle(paste0(unique(df$esp_nom_commun), " (",
                                      unique(df$esp_code_alternatif), ")")) +
              ggplot2::coord_flip()
          }
          # Save boxplot
          if(plot == TRUE & save == TRUE) {
            ggplot2::ggsave(filename =
                              paste0(unique(df$esp_code_alternatif),
                                     "_bxp", ".png"),
                            path = paste0(dir, "/", newfolder, "/"))
          }
          # Create map
          if(map == TRUE) {
            g2 <- df %>%
              dplyr::filter(effectif_sptype == "alerte") %>%
              ggplot2::ggplot() +
              ggplot2::geom_sf(data = map.contour, colour = "grey60",
                               fill = "grey85") +
              ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
              ggplot2::geom_point(aes(x = pop_coordonnees_x,
                                      y = pop_coordonnees_y,
                                      color = sum_effectif) , size = 2) +
              viridis::scale_color_viridis(option = "magma", direction = -1) +
              ggplot2::ggtitle(paste0(unique(df$esp_nom_commun), " (",
                                      unique(df$esp_code_alternatif), ")")) +
              ggplot2::labs(x = "Longitude", y = "Latitude")
          }
          # Save map
          if (map == TRUE & save == TRUE) {
            ggplot2::ggsave(filename =
                              paste0(unique(df$esp_code_alternatif),
                                     "_map", ".png"),
                            path = paste0(dir, "/", newfolder, "/"))
          }
          # Print map and/or boxplot
          if(plot == TRUE & map == FALSE) {
            print(g1)
          } else if (map == TRUE & plot == FALSE) {
            print(g2)
          } else if (map == TRUE & plot == TRUE) {
            gridExtra::grid.arrange(g1, g2, ncol = 2)
          }
        }
      )
    commentaire <- paste0("Alertes effectifs aberrants IQR criterion")

    # ===================
    # Effectif par espece

  } else if (echelle == "espece") {
    data_reduce <- data %>%
      dplyr::select(ope_id,
                    lop_id,
                    esp_code_alternatif,
                    lop_effectif) %>%
      unique()

    data_sum_ope <- data_reduce %>%
      dplyr::group_by(ope_id, esp_code_alternatif) %>%
      dplyr::summarise(sum_ope = sum(lop_effectif, na.rm = TRUE)) %>%
      dplyr::left_join(data_reduce, .)

    data_effectif <- data_sum_ope %>%
      dplyr::group_by(esp_code_alternatif) %>%
      dplyr::summarise(
        min_IQR = quantile(sum_ope, probs = 0.25, na.rm = TRUE) -
          (1.5*IQR(sum_ope, na.rm = TRUE)),
        max_IQR = quantile(sum_ope, probs = 0.75, na.rm = TRUE) +
          (1.5*IQR(sum_ope, na.rm = TRUE)),
        moy_effectif = round(mean(sum_ope, na.rm = TRUE),
                             digits = 0)) %>%
      dplyr::left_join(data_sum_ope, .) %>%
      dplyr::select(ope_id,
                    esp_code_alternatif,
                    sum_ope,
                    min_IQR,
                    max_IQR,
                    moy_effectif) %>%
      unique() %>%
      dplyr::mutate(
        effectif_spmessage = dplyr::case_when(
          is.na(esp_code_alternatif) == TRUE ~ "espece manquante",
          is.na(sum_ope) == TRUE ~ "effectif manquant",
          sum_ope < min_IQR ~ "effectif trop faible",
          sum_ope > max_IQR ~ "effectif trop eleve",
          sum_ope > min_IQR & sum_ope < max_IQR ~ "effectif normal"
        ),
        effectif_sptype = dplyr::case_when(
          is.na(esp_code_alternatif) == TRUE ~ "alerte",
          is.na(sum_ope) == TRUE ~ "alerte",
          sum_ope < min_IQR ~ "alerte",
          sum_ope > max_IQR ~ "alerte",
          sum_ope > min_IQR & sum_ope < max_IQR ~ ""
        )
      )
    commentaire <- paste0("Alertes effectifs aberrants : </> à ",
                          percent, "% du min/max")

    # Create boxplot
    if (plot == TRUE) {
      g1 <- ggplot2::ggplot(data_effectif,
                            aes(x = forcats::fct_reorder(esp_code_alternatif,
                                                         moy_effectif),
                                y = sum_ope)) +
        ggplot2::geom_boxplot() +
        ggplot2::geom_point(aes(color = effectif_type)) +
        ggplot2::theme(axis.text.y  = element_text(hjust = 0.1, size = 6)) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(values = pal_alerte) +
        ggplot2::labs(x = "Code espece", y = "Effectifs par operation")
    }
  }

  # Statistics
  if (stat == TRUE) {
    effectif_stat <- data_effectif %>%
      plyr::summarise(
        na.sp = round(
          mean(effectif_spmessage == "espece manquante", na.rm = T) * 100,
          digits = 2),
        na.eff = round(
          mean(effectif_spmessage == "effectif manquant", na.rm = T) * 100,
          digits = 2),
        alerte = round(
          mean(effectif_sptype == "alerte", na.rm = T) * 100, digits = 2),
        effectif_faible = round(
          mean(effectif_spmessage == "effectif trop faible", na.rm = T) * 100,
          digits = 2),
        effectif_eleve = round(
          mean(effectif_spmessage == "effectif trop eleve", na.rm = T) * 100,
          digits = 2),
        effectif_normal = round(
          mean(effectif_spmessage == "effectif normal", na.rm = T) * 100,
          digits = 2))

    effectif_nb <- data_effectif %>%
      plyr::summarise(
        na.sp = sum(effectif_spmessage == "espece manquante", na.rm = T),
        na.eff = sum(effectif_spmessage == "effectif manquant", na.rm = T),
        alerte = sum(effectif_sptype == "alerte", na.rm = T),
        effectif_faible = sum(effectif_spmessage == "effectif trop faible",
                              na.rm = T),
        effectif_eleve = sum(effectif_spmessage == "effectif trop eleve",
                             na.rm = T),
        effectif_normal = sum(effectif_spmessage == "effectif normal",
                              na.rm = T))

    # Create tables
    table_stat <- make_tab(x = effectif_stat,
                           colours = c("#BF3111", "#BF3111", "#BF3111",
                                       "#BF3111", "#BF3111", "#5AB5BF"),
                           titre = "Rapport effectif espèces (%)",
                           comment = commentaire, size.tab = 9)

    table_nb <- make_tab(x = effectif_nb,
                         colours = c("#BF3111", "#BF3111", "#BF3111",
                                     "#BF3111", "#BF3111", "#5AB5BF"),
                         titre = "Rapport effectif espèces(nb)",
                         comment = commentaire, size.tab = 9)
  }

  data_effectif <- data_effectif %>%
    dplyr::group_by(ope_id) %>%
    dplyr::group_split() %>%
    purrr::map_df(
      .f = function(df) {
        df %>%
          dplyr::mutate(
            effectif_type = dplyr::case_when(
              any(stringr::str_detect(effectif_sptype, "alerte")) == TRUE ~
                "alerte",
              any(stringr::str_detect(effectif_sptype, "alerte")) == FALSE ~
                ""
            )
          )
      }
    )

  # create pdf files
  if (pdf == TRUE & echelle == "espece") {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g1) # boxplot
    }
    if (stat == TRUE) {
      grid::grid.newpage()
      grid::grid.draw(table_stat) # percent
      grid::grid.newpage()
      grid::grid.draw(table_nb) # nb
    }
    if (stat == FALSE & plot == FALSE) {
      grid::grid.newpage()
    }
    grDevices::dev.off()
  }

  # Plot statistics
  if (stat == TRUE) {
    grid::grid.newpage()
    grid::grid.draw(table_stat) # percent
    grid::grid.newpage()
    grid::grid.draw(table_nb) # nb
  } else {
    print("no statistic draw ==> write stat = TRUE to see it")
  }
  # Plot the boxplot
  if (plot == TRUE & echelle == "espece") {
    print(g1)
  } else {
    print("no plot draw ==> write plot = TRUE to see it")
  }
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_effectif, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_effectif)
}
