# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Occurence des especes
#' @description Fonction de visualisation de la frequence d'apparition (occurrence) des especes dans les operations.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "sp_occ"`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot du pourcentage d'occurrence de chaque espece est trace.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param color.point Vecteur. Palette contenant les couleurs pour les differentes categories d'occurrence (espece commune, espece peu commune, espece rare et espece non renseignee). Par defaut, `color.point = colorblind_8`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @return Des resultats graphiques : carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Une fonction de visualisation de la distribution spatiale et des effectifs par espece : \code{\link{distri_sp}}.
#'
#' @examples
#' result_occ <- test_sp_occ(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE
#' )

#' @importFrom dplyr n_distinct group_by summarise mutate case_when inner_join filter
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot theme element_text coord_flip scale_fill_manual geom_col labs geom_sf geom_point scale_color_manual ggtitle
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_sp_occ <- function(data,
                        stat = TRUE,
                        dir = getwd(),
                        file.name = "sp_occ",
                        plot = TRUE,
                        map = TRUE,
                        csv = TRUE,
                        color_point = colorblind_8,
                        map.contour = IDF_contour,
                        map.cours.eau = cours_eau,
                        pdf = TRUE) {

  ope <- data %>%
    dplyr::select(ope_id, esp_code_alternatif)

  # Occurence par espece
  nb_ope <- dplyr::n_distinct(data$ope_id)
  data_freq <- data %>%
    dplyr::group_by(esp_code_alternatif) %>%
    dplyr::summarise(freq = round(n_distinct(ope_id) / nb_ope * 100,
                                  digits = 2)) %>%
    dplyr::mutate(
      occ_spmessage = dplyr::case_when(
        is.na(esp_code_alternatif) == TRUE ~ "espece manquante",
        freq >= 20 ~ "espece commune",
        freq < 20 & freq >= 1 ~ "espece peu commune",
        freq < 1 ~ "espece rare"
      ),
      occ_sptype = dplyr::case_when(
        is.na(esp_code_alternatif) == TRUE ~ "alerte",
        freq >= 20 ~ "",
        freq < 20 & freq >= 1 ~ "",
        freq < 1 ~ "alerte"
      )
    )

  # Create barplot
  if (plot == TRUE) {
    g1 <- ggplot2::ggplot(data_freq,
                          aes(x = forcats::fct_reorder(esp_code_alternatif, freq),
                              y = freq,
                              fill = occ_spmessage)) +
      ggplot2::theme(axis.text.y  = ggplot2::element_text(
        hjust = 0.1, size = 6)) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = color_point) +
      ggplot2::geom_col(alpha = .6, width = .7) +
      ggplot2::labs(x = "Code espece", y = "Occurence (%)")
  }

  # Create the map
  if (map == TRUE) {
    g2 <- data %>%
      dplyr::inner_join(data_freq) %>%
      dplyr::filter(occ_sptype == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = IDF_contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = cours_eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = as.factor(occ_spmessage)),
                          size = 2) +
      ggplot2::scale_color_manual(values = color_point) +
      ggplot2::ggtitle("Carte des espèces rares et NA") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Type d'espèces")
  }

  # Statistics
  if (stat == TRUE) {
    occ_stat <- data_freq %>%
      plyr::summarise(
        na = round(
          mean(occ_spmessage == "espece manquante", na.rm = T) * 100,
          digits = 2),
        alerte = round(
          mean(occ_sptype == "alerte", na.rm = T) * 100, digits = 2),
        sp_commune = round(
          mean(occ_spmessage == "espece commune", na.rm = T) * 100,
          digits = 2),
        sp_peu_commune = round(
          mean(occ_spmessage == "espece peu commune", na.rm = T) * 100,
          digits = 2),
        sp_rare = round(
          mean(occ_spmessage == "espece rare", na.rm = T) * 100, digits = 2))

    occ_nb <- data_freq %>%
      plyr::summarise(
        na = sum(occ_spmessage == "espece manquante", na.rm = T),
        alerte = sum(occ_sptype == "alerte", na.rm = T),
        sp_commune = sum(occ_spmessage == "espece commune", na.rm = T),
        sp_peu_commune = sum(occ_spmessage == "espece peu commune", na.rm = T),
        sp_rare = sum(occ_spmessage == "espece rare", na.rm = T))

    # Create tables
    commentaire <- c("Alertes pour des espèces rares (occ < 1%)")

    table_stat <- make_tab(x = occ_stat,
                           colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                       "#5AB5BF", "#BF3111"),
                           titre = "Rapport occurrence espèces (%)",
                           comment = commentaire, size.tab = 7)
    table_nb <- make_tab(x = occ_nb,
                         colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                     "#5AB5BF", "#BF3111"),
                         titre = "Rapport occurrence espèces(nb)",
                         comment = commentaire, size.tab = 7)
  }

  data_freq <- data_freq %>%
    left_join(ope, .)

  data_freq <- data_freq %>%
    dplyr::group_by(ope_id) %>%
    dplyr::group_split() %>%
    purrr::map_df(
      .f = function(df) {
        df %>%
          dplyr::mutate(
            occ_type = dplyr::case_when(
              any(stringr::str_detect(occ_sptype, "alerte")) == TRUE ~
                "alerte",
              any(stringr::str_detect(occ_sptype, "alerte")) == FALSE ~
                ""
            )
          )
      }
    )

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g1) # boxplot
    }
    if (map == TRUE) {
      print(g2) # map
    }
    if (stat == TRUE) {
      grid::grid.newpage()
      grid::grid.draw(table_stat) # percent
      grid::grid.newpage()
      grid::grid.draw(table_nb) # nb
    }
    if (stat == FALSE & map == FALSE & plot == FALSE) {
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
  # Plot the barplot
  if (plot == TRUE) {
    print(g1)
  } else {
    print("no plot draw ==> write plot = TRUE to see it")
  }
  # Plot the map
  if (map == TRUE) {
    print(g2)
  } else {
    print("no map draw ==> write map = TRUE to see it")
  }
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_freq, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_freq)
}

# ===========================
#' Distribution des especes
#' @description Fonction de visualisation des effectifs par stations (boxplot) et de la distribution des especes sur la zone concernees.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param folder Dossier ou enregistrer les cartes et les boxplots. Si `folder = NULL`, un nouveau dossier ("distribution_sp") est cree dans `dir`.
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "distri_sp"`.
#' @param save Argument logique `TRUE/FALSE`. Par defaut `save = FALSE` et les graphiques produits ne seront pas enregistres dans un repertoire local.
#' @param boxplot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot des effectifs aux stations par espece est trace.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @return Des resultats graphiques : carte (`map = TRUE`) et boxplot (`boxplot = TRUE`).
#' @export
#'
#' @seealso Une fonction de visualisation de la frequence d'apparition (occurrence) des especes dans les operations : \code{\link{test_sp_occ}}.
#'
#' @examples
#' result_distri <- distri_sp(
#' data = exemple_IDF,
#' save = FALSE,
#' boxplot = TRUE,
#' map = TRUE)

#' @importFrom dplyr select group_by summarise filter group_split
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_boxplot geom_point labs ggtitle coord_flip ggsave geom_sf
#' @importFrom gridExtra grid.arrange
#' @importFrom purrr map
#' @importFrom stats median
#' @importFrom viridis scale_color_viridis

distri_sp <- function(data,
                      dir = getwd(),
                      folder = NULL,
                      file.name = "distri_sp",
                      save = FALSE,
                      boxplot = TRUE,
                      map = TRUE,
                      map.contour = IDF_contour,
                      map.cours.eau = cours_eau) {

  # Create a new folder
  if (save == TRUE) {
    if (is.null(folder) == TRUE) {
      newfolder <- "distribution_sp"
      dir.create(file.path(dirname(dir), newfolder))
    } else {
      newfolder <- folder
    }
  }

  # Effectif de chaque espece par operation
  data_distri <- data %>%
    dplyr::select(pop_id, ope_id, esp_code_alternatif, lop_id, lop_effectif,
                  esp_nom_commun, ope_surface_calculee, pop_coordonnees_x,
                  pop_coordonnees_y) %>%
    unique() %>%
    dplyr::group_by(ope_id, esp_code_alternatif, esp_nom_commun, pop_id,
                    pop_coordonnees_x, pop_coordonnees_y) %>%
    dplyr::summarise(sum_effectif = sum(lop_effectif))

  data_pop_med <- data_distri %>%
    dplyr::group_by(pop_id, esp_code_alternatif) %>%
    dplyr::summarise(median_pop = round(median(sum_effectif, na.rm = T),
                                        digits = 0))

  data_distri <- left_join(data_distri, data_pop_med)

  # Create one boxplot for each specie
  plot_distri <- data_distri %>%
    dplyr::filter(is.na(esp_code_alternatif) == FALSE) %>%
    dplyr::group_by(esp_code_alternatif) %>%
    dplyr::group_split() %>%
    purrr::map(
      .f = function(df) {
        # Create boxplot
        if(boxplot == TRUE) {
          g1 <- ggplot2::ggplot(
            aes(x = fct_reorder(as.factor(pop_id), median_pop),
                y = sum_effectif), data = df) +
            ggplot2::geom_boxplot() +
            ggplot2::geom_point() +
            ggplot2::labs(x = "Point de prélèvement", y = "Effectif") +
            ggplot2::ggtitle(paste0(unique(df$esp_nom_commun), " (",
                                    unique(df$esp_code_alternatif), ")")) +
            ggplot2::coord_flip()
        }
        # Save boxplot
        if(boxplot == TRUE & save == TRUE) {
          ggplot2::ggsave(filename =
                            paste0(unique(df$esp_code_alternatif),
                                   "_bxp", ".png"),
                          path = paste0(dir, "/", newfolder, "/"))
        }
        # Create map
        if(map == TRUE) {
          g2 <- df %>%
            ggplot2::ggplot() +
            ggplot2::geom_sf(data = map.contour, colour = "grey60",
                             fill = "grey85") +
            ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
            ggplot2::geom_point(aes(x = pop_coordonnees_x,
                                    y = pop_coordonnees_y,
                                    color = median_pop) , size = 2) +
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
        if(boxplot == TRUE & map == FALSE) {
          print(g1)
        } else if (map == TRUE & boxplot == FALSE) {
          print(g2)
        } else if (map == TRUE & boxplot == TRUE) {
          gridExtra::grid.arrange(g1, g2, ncol = 2)
        }
        # Return
        # if (boxplot == TRUE) {
        # return(g1)
        # }
        # if (map == TRUE) {
        # return(g2)
        #
      }
    )
  return(data_distri)
}

# ===========================
#' Richesse specifique aux points de prelevement
#' @description Fonction de visualisation de la richesse specifique aux points de prelevements au cours du temps.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param folder Dossier ou enregistrer les cartes et les boxplots. Si `folder = NULL`, un nouveau dossier ("distribution_sp") est cree dans `dir`.
#' @param save Argument logique `TRUE/FALSE`. Par defaut `save = FALSE` et les graphiques produits ne seront pas enregistres dans un repertoire local.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un graphique de la richesse specifique au cours du temps pour chaque point de prelevement est trace.
#' @return Des resultats graphiques si `plot = TRUE`.
#'
#' @examples
#' diversite <- pop_diversite(
#' data = IDF_new,
#' plot = TRUE)

#' @importFrom dplyr select filter mutate group_by group_split
#' @importFrom ggplot2 ggplot geom_point ggtitle ylim labs ggsave
#' @importFrom purrr map

pop_diversite <- function(data,
                          dir = getwd(),
                          folder = NULL,
                          save = FALSE,
                          plot = TRUE
) {

  # Create a new folder
  if (save == TRUE) {
    if (is.null(folder) == TRUE) {
      newfolder <- "pop_diversite"
      dir.create(file.path(dirname(dir), newfolder))
    } else {
      newfolder <- folder
    }
  }

  # Create graphiques
  nb_sp <- n_distinct(data$esp_code_alternatif)

  data_div <- data %>%
    dplyr::select(ope_jour, pop_id, esp_code_alternatif,
                  esp_nom_commun, ope_id) %>%
    unique() %>%
    dplyr::filter(is.na(esp_code_alternatif) == FALSE) %>%
    dplyr::mutate(annee = format(ope_jour, format = "%Y")) %>%
    dplyr::group_by(annee, pop_id) %>%
    dplyr::mutate(div_annees = dplyr::n_distinct(esp_code_alternatif)) %>%
    dplyr::group_by(pop_id) %>%
    dplyr::group_split() %>%
    purrr::map(
      .f = function(df) {
        point <- unique(df$pop_id)
        g1 <-
          ggplot2::ggplot(df, aes(x = annee,
                                  y = div_annees)) +
          ggplot2::geom_point() +
          ggplot2::ggtitle(point) +
          ggplot2::ylim(0, nb_sp) +
          ggrepel::geom_label_repel(aes(label = as.character(ope_id)),
                                    box.padding   = 0.35,
                                    point.padding = 0.5,
                                    segment.color = 'grey50') +
          ggplot2::labs(x = "Années", y = "Richesse spécifique")

        print(g1)

        # Save plot
        if(plot == TRUE & save == TRUE) {
          ggplot2::ggsave(filename =
                            paste0(unique(df$pop_id),
                                   "_plot", ".png"),
                          path = paste0(dir, "/", newfolder, "/"))
        }
      }

    )
}

# ===========================
#' Occurrences des especes par an
#' @description Fonction de visualisation de la frequence d'apparition (Occurrence) des especes au cours du temps.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param folder Dossier ou enregistrer les cartes et les boxplots. Si `folder = NULL`, un nouveau dossier ("Occ_Annees") est cree dans `dir`.
#' @param save Argument logique `TRUE/FALSE`. Par defaut `save = FALSE` et les graphiques produits ne seront pas enregistres dans un repertoire local.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un graphique des occurrences par espece est trace.
#' @return Des resultats graphiques si `plot = TRUE`.
#'
#' @examples
#' occurrence_annees <- Occ_Annees(
#' data = IDF_new,
#' plot = TRUE)

#' @importFrom dplyr select filter mutate group_by n_distinct group_split
#' @importFrom ggplot2 ggplot geom_point ggtitle ylim labs ggsave
#' @importFrom purrr map

Occ_Annees <- function(data,
                       dir = getwd(),
                       folder = NULL,
                       save = FALSE,
                       plot = TRUE) {

  # Create a new folder
  if (save == TRUE) {
    if (is.null(folder) == TRUE) {
      newfolder <- "Occ_Annees"
      dir.create(file.path(dirname(dir), newfolder))
    } else {
      newfolder <- folder
    }
  }

  # Create plot
  data_OccAnnees <- data %>%
    dplyr::select(ope_jour,
                  ope_id,
                  esp_code_alternatif,
                  esp_nom_commun) %>%
    unique() %>%
    dplyr::filter(is.na(esp_code_alternatif) == FALSE) %>%
    dplyr::mutate(annee = format(ope_jour, format = "%Y")) %>%
    dplyr::group_by(annee) %>%
    dplyr::mutate(nb_ope_annees = dplyr::n_distinct(ope_id)) %>%
    dplyr::group_by(esp_code_alternatif, annee) %>%
    dplyr::mutate(occ_annees = round(n_distinct(ope_id) /
                                       nb_ope_annees * 100,
                                     digits = 2)) %>%
    dplyr::group_by(esp_code_alternatif) %>%
    dplyr::group_split() %>%
    purrr::map(
      .f = function(df) {
        sp <- unique(df$esp_nom_commun)
        g1 <-
          ggplot2::ggplot(df, aes(x = annee,
                                  y = occ_annees)) +
          ggplot2::geom_point() +
          ggplot2::ggtitle(sp) +
          ggplot2::ylim(0, 100) +
          ggplot2::labs(x = "Annees", y = "Occurence (%)")
        print(g1)

        # Save plot
        if(plot == TRUE & save == TRUE) {
          ggplot2::ggsave(filename =
                            paste0(unique(df$esp_code_alternatif),
                                   "_plot", ".png"),
                          path = paste0(dir, "/", newfolder, "/"))
        }
      }
    )
}

# ===========================
