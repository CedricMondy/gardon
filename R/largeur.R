# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de largeur de la lame d'eau
#' @description Quatre methodes disponibles afin de tester la coherence des valeurs de largeur.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_Lar"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Quatre methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers. Si `method = 4`, l'IQR est cette fois applique en groupant les operations par point de prelevement.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color.alerte = pal_alerte`.
#' @param color.station Vecteur. Contient les couleurs des stations. Par defaut, `color_station = RandomCol_74`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Pourcentage d'ecart aux valeurs seuils si `method = 2`.
#' @param echelle Deux valeurs possibles. Si `echelle = "type_cours_eau"` alors les valeurs aberrantes sont estimees a l'echelle des types de cours d'eau (voir \code{\link{getDataRegion}} pour inclure type de cours d'eau). Si `echelle = "POP"` alors les valeurs aberrantes sont estimees a l'echelle des points de prelevement.

#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Une fonction dependante des resultats de `test_ope_Lar()` : \code{\link{test_ope_Lon}}. Une fonction exploitant la relation entre la largeur, la profondeur et la longueur de l'operation : \code{\link{test_ope_mesure}}.
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2` ou `method = 4`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @examples
#' result_Lar <- test_ope_Lar(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 4)
#'
#' @importFrom dplyr select mutate case_when group_by group_split first last inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point labs scale_color_manual aes coord_flip ggtitle geom_sf
#' @importFrom grDevices boxplot.stats pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom purrr map_df map
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis

test_ope_Lar <- function(data,
                         plot = TRUE,
                         stat = TRUE,
                         min = 100,
                         max = 2000,
                         dir = getwd(),
                         file.name = "ope_Lar",
                         map = TRUE,
                         csv = TRUE,
                         method = 3,
                         confidence.interval = 95,
                         color.alerte = pal_alerte,
                         color.station = RandomCol_74,
                         map.contour = IDF_contour,
                         map.cours.eau = cours_eau,
                         pdf = TRUE,
                         percent = NULL,
                         echelle = "POP") {

  # Identify outliers according to an interval
  if (method == 1 | method == 2 | method == 3) {
    if (method == 1) {
      min_value_Lar <- min
      max_value_Lar <- max

      # Identify outliers with IQR criterion
    } else if (method == 2) {
      data_Lar <- data %>%
        dplyr::select(ope_id, odp_largeur_lame_eau, ope_pro_id, pop_id,
                      enh_libelle_sandre) %>%
        unique()

      min_value_Lar <- quantile(
        data_Lar$odp_largeur_lame_eau, probs = 0.25, na.rm = TRUE) -
        1.5*IQR(data_Lar$odp_largeur_lame_eau, na.rm = TRUE)
      max_value_Lar <- quantile(
        data_Lar$odp_largeur_lame_eau, probs = 0.75, na.rm = TRUE) +
        1.5*IQR(data_Lar$odp_largeur_lame_eau, na.rm = TRUE)

      if(is.null(percent) == FALSE) {
        min_value_Lar <- min_value_Lar - min_value_Lar * percent / 100
        max_value_Lar <- max_value_Lar + max_value_Lar * percent / 100
      }

      # Identify outliers with percentile method with interval of confidence
    } else if (method == 3) {
      num_lower <- ((100 - confidence.interval) / 2) / 100
      num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
      min_value_Lar <- stats::quantile(
        data$odp_largeur_lame_eau, num_lower, na.rm = TRUE)
      max_value_Lar <- stats::quantile(
        data$odp_largeur_lame_eau, num_upper, na.rm = TRUE)
    }
    commentaire <- paste0("Alertes en dehors de l'intervale [",
                          min_value_Lar, "; ", max_value_Lar, "]")
    print(commentaire)

    data_Lar <- data %>%
      dplyr::select(ope_id, odp_largeur_lame_eau, ope_pro_id, pop_id,
                    enh_libelle_sandre) %>%
      unique() %>%
      dplyr::mutate(
        Lar_message = dplyr::case_when(
          is.na(odp_largeur_lame_eau) == TRUE ~ "Largeur manquante",
          min_value_Lar <= odp_largeur_lame_eau &
            odp_largeur_lame_eau <= max_value_Lar ~ "Largeur normale",
          min_value_Lar > odp_largeur_lame_eau |
            odp_largeur_lame_eau > max_value_Lar ~ "Largeur anormale"
        ),
        Lar_type = dplyr::case_when(
          is.na(odp_largeur_lame_eau) == TRUE ~ "alerte",
          min_value_Lar <= odp_largeur_lame_eau &
            odp_largeur_lame_eau <= max_value_Lar ~ "",
          min_value_Lar > odp_largeur_lame_eau |
            odp_largeur_lame_eau > max_value_Lar ~ "alerte",
          TRUE ~ ""
        )
      )
    if (plot == TRUE) {
      g <- data_Lar %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = factor(0), y = odp_largeur_lame_eau)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Lar_type)) +
        ggplot2::labs(x = "", y = "Largeur lame eau", color = "alerte") +
        ggplot2::scale_color_manual(values = color.alerte)

      g2 <- data_Lar %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = as.factor(ope_pro_id),
                            y = odp_largeur_lame_eau)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Lar_type)) +
        ggplot2::labs(x = "Type protocole", y = "Largeur lame eau",
                      color = "alerte") +
        ggplot2::scale_color_manual(values = color.alerte)

      g3 <- data_Lar %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = as.factor(enh_libelle_sandre),
                            y = odp_largeur_lame_eau)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Lar_type)) +
        coord_flip() +
        ggplot2::labs(x = "Nom cours eau", y = "Largeur lame eau",
                      color = "alerte") +
        ggplot2::scale_color_manual(values = color.alerte)
    }

    # Identify outliers with IQR criterion by point prelevement
  } else if (method == 4) {

    if (echelle == "POP") {
      var <- dplyr::quo(pop_id)
      typ_eau <- 0
      legende_x <- "Point de prélèvement"
    } else if (echelle == "type_cours_eau") {
      var <- dplyr::quo(Type_cours_eau)
      typ_eau <- c("Type_cours_eau")
      legende_x <- "Type de cours d'eau"
    } else if (echelle != "POP" | echelle != "type_cours_eau") {
      stop("echelle inconnue : choisir entre POP ou type_cours_eau")
    }

    data_Lar <- data %>%
      dplyr::select(ope_id,
                    odp_largeur_lame_eau,
                    ope_pro_id,
                    pop_id,
                    enh_libelle_sandre,
                    sta_code_sandre,
                    all_of(typ_eau)) %>%
      unique() %>%
      dplyr::group_by(!!var) %>%
      dplyr::group_split() %>%
      purrr::map_df(
        .f = function(df1) {
          min_value_Lar <- quantile(
            df1$odp_largeur_lame_eau, probs = 0.25, na.rm = TRUE) -
            1.5*IQR(df1$odp_largeur_lame_eau, na.rm = TRUE)

          max_value_Lar <- quantile(
            df1$odp_largeur_lame_eau, probs = 0.75, na.rm = TRUE) +
            1.5*IQR(df1$odp_largeur_lame_eau, na.rm = TRUE)

          if(is.null(percent) == FALSE) {
            min_value_Lar <- min_value_Lar - min_value_Lar * percent / 100
            max_value_Lar <- max_value_Lar + max_value_Lar * percent / 100
          }

          df1 <- df1 %>%
            dplyr::mutate(
              Lar_message = dplyr::case_when(
                is.na(odp_largeur_lame_eau) == TRUE ~ "Largeur manquante",
                min_value_Lar <= odp_largeur_lame_eau &
                  odp_largeur_lame_eau <= max_value_Lar ~ "Largeur normale",
                min_value_Lar > odp_largeur_lame_eau |
                  odp_largeur_lame_eau > max_value_Lar ~ "Largeur anormale"
              ),
              Lar_type = dplyr::case_when(
                is.na(odp_largeur_lame_eau) == TRUE ~ "alerte",
                min_value_Lar <= odp_largeur_lame_eau &
                  odp_largeur_lame_eau <= max_value_Lar ~ "",
                min_value_Lar > odp_largeur_lame_eau |
                  odp_largeur_lame_eau > max_value_Lar ~ "alerte",
                TRUE ~ ""
              ),
              ecart_Lar = dplyr::case_when(
                Lar_message == "Largeur manquante" ~ NA_real_,
                Lar_message == "Largeur normale" ~ 0,
                Lar_message == "Largeur anormale" &
                  odp_largeur_lame_eau > max_value_Lar ~
                  odp_largeur_lame_eau - max_value_Lar,
                Lar_message == "Largeur anormale" &
                  odp_largeur_lame_eau < min_value_Lar ~
                  min_value_Lar - odp_largeur_lame_eau
              )
            )
        }
      )

    # create boxplot
    if (plot == TRUE) {
      g4 <- data_Lar %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = as.factor(!!var),
                            y = odp_largeur_lame_eau)) +
        #fill = sta_code_sandre)) +
        ggplot2::geom_boxplot() +
        #ggplot2::scale_fill_manual(values = color.station) +
        ggplot2::geom_point(aes(color = Lar_type), size = 1) +
        ggplot2::labs(x = legende_x, y = "Largeur lame eau") +
        ggplot2::scale_color_manual(values = color.alerte) +
        ggplot2::coord_flip()

      # Create boxplot largeur for each station
      # lar_sta <- data_Lar %>%
      # dplyr::group_by(sta_code_sandre) %>%
      # dplyr::group_split() %>%
      # purrr::map(
      #   .f = function(df) {
      #     # Create boxplot
      #     g_sta <- ggplot2::ggplot(aes(x = as.factor(pop_id),
      #                               y = odp_largeur_lame_eau),
      #                              data = df) +
      #       ggplot2::geom_boxplot() +
      #       ggplot2::geom_point(aes(color = Lar_type), size = 1) +
      #       ggplot2::scale_color_manual(values = color.alerte) +
      #       ggplot2::labs(x = "Point de prélèvement", y = "Largeur lame eau") +
      #       ggplot2::ggtitle(paste0("Station Sandre : ",
      #                               unique(df$sta_code_sandre), " (",
      #                               unique(df$enh_libelle_sandre), ")")) +
      #       ggplot2::coord_flip()
      #     return(g_sta)
      #   }
      # )

      g5 <- data_Lar %>%
        ggplot2::ggplot(aes(x = factor(0),
                            y = ecart_Lar)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point() +
        ggplot2::labs(x = "", y = "Largeur lame eau") +
        ggplot2::ggtitle("Boxplot des écarts au max/min des alertes")

      commentaire <- "alerte IQR criterion par points de prelevement"
    }
  } else {
    stop("method inconnue")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_Lar) %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(Lar_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = odp_largeur_lame_eau), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des valeurs de largeur en alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Largeur lame eau")
  }

  # Create statistics
  if (stat == TRUE) {
    Lar_stat <- data_Lar %>% #percent
      plyr::summarise(
        na = round(
          mean(Lar_message == "Largeur manquante") * 100,
          digits = 2),
        alerte = round(
          mean(Lar_type == "alerte") * 100, digits = 2),
        Lar.anormale = round(
          mean(Lar_message == "Largeur anormale")* 100,
          digits = 2),
        Lar.normale = round(mean(
          Lar_message == "Largeur normale") * 100, digits = 2))

    Lar_nb <- data_Lar %>% #number
      plyr::summarise(
        na = sum(Lar_message == "Largeur manquante"),
        alerte = sum(Lar_type == "alerte"),
        Lar.anormale = sum(Lar_message == "Largeur anormale"),
        Lar.normale = sum(Lar_message == "Largeur normale"))

    # create tables
    table_stat <- make_tab(x = Lar_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Largeur (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = Lar_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Largeur (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      if (method == 1 | method == 2 | method == 3) {
        print(g) # boxplot
        print(g2) # boxplot protocole
        print(g3) # boxplot cours eau
      } else if (method == 4) {
        print(g4) # boxplot point prelevement
        print(g5) # boxplot des ecarts
        # print(lar_sta) # bexplot par station
      }
    }
    if (map == TRUE) {
      print(g1) # map
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
  # Plot the boxplot
  if (plot == TRUE) {
    if (method == 1 | method == 2 | method == 3) {
      grid::grid.newpage()
      print(g) # boxplot
      grid::grid.newpage()
      print(g2) # boxplot protocole
      grid::grid.newpage()
      print(g3) # boxplot cours eau
    } else if (method == 4) {
      print(g4)# boxplot point prelevement
      grid::grid.newpage()
      print(g5) # boxplot des ecarts
      grid::grid.newpage()
      # print(lar_sta) # bexplot par station
    }
  } else {
    print("no plot draw ==> write plot = TRUE to see it")
  }
  # Plot the map
  if (map == TRUE) {
    print(g1)
  } else {
    print("no map draw ==> write map = TRUE to see it")
  }
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_Lar, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_Lar)
}
