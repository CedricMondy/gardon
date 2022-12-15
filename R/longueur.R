# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de longueur de l'operation
#' @description Quatre methodes disponibles afin de tester la coherence des valeurs de longueur.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_Lon"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Quatre methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers. Si `method = 4`, une courbe de tendance entre la longueur et largeur est tracee et les outliers sont identifie selon un pourcentage d'ecart a cette tendance.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param percent Pourcentage d'ecart a la tendance a prendre en compte si `method = 4` et pourcentage d'ecart aux valeurs seuils si `method = 2`.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Attention ! Si `method = 4`, on compare les valeurs de longueur a celle de largeur. Il est donc conseille de regarder egalement les resultats de la fonction : \code{\link{test_ope_Lar}}. Une fonction exploitant la relation entre la largeur, la profondeur et la longueur de l'operation : \code{\link{test_ope_mesure}}.
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#' @examples
#' result_Lon <- test_ope_Lon(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 4)
test_ope_Lon <- function(data,
                         plot = TRUE,
                         stat = TRUE,
                         min = 100,
                         max = 1500,
                         dir = getwd(),
                         file.name = "ope_Lon",
                         map = TRUE,
                         csv = TRUE,
                         method = 3,
                         confidence.interval = 95,
                         percent = 50,
                         color.alerte = pal_alerte,
                         map.contour = IDF_contour,
                         map.cours.eau = cours_eau,
                         pdf = TRUE) {

  # Autre methode en tracant relation avec la distance a la source
  if (method == 4){
    # select variables of interrest
    df <- data %>%
      select(ope_id, odp_largeur_lame_eau, odp_longueur,
             enh_libelle_sandre) %>%
      unique()
    # run loess model
    df.lo <- loess(odp_longueur ~ odp_largeur_lame_eau, df)
    # Get predictions ==> fit and se.fit vectors
    pred <- predict(df.lo, se = TRUE)
    # create a data.frame from those
    df2 <- data.frame(largeur = df$odp_largeur_lame_eau,
                      fit_Lon = pred$fit,
                      #se.fit = pred$se.fit * qt(0.95 / 2 + .5, pred$df),
                      se.fit_Lon = pred$fit * percent / 100,
                      interval_max_Lon = pred$fit + pred$fit * percent / 100,
                      interval_min_Lon = pred$fit - pred$fit * percent / 100,
                      ope_id = df$ope_id)


    # make the flag
    outerpoints <- +(df$odp_longueur >
                       df2$fit_Lon + df2$se.fit_Lon |
                       df$odp_longueur <
                       df2$fit_Lon - df2$se.fit_Lon)

    #add flag to original data frame
    df$outer_Lon <- outerpoints
    df <- df %>%
      dplyr::inner_join(df2, by = "ope_id") %>%
      dplyr::mutate(
        ecart_Lon = dplyr::case_when(
          odp_longueur > fit_Lon + se.fit_Lon ~
            round(odp_longueur - (fit_Lon + se.fit_Lon)),
          odp_longueur < fit_Lon - se.fit_Lon ~
            round((fit_Lon - se.fit_Lon) - odp_longueur),
          TRUE ~ 0
        )
      ) %>%
      dplyr::select(ope_id, odp_largeur_lame_eau,
                    odp_longueur, fit_Lon, se.fit_Lon,
                    outer_Lon, ecart_Lon, enh_libelle_sandre,
                    interval_max_Lon, interval_min_Lon)

    data_Lon <- df %>%
      dplyr::mutate(
        Lon_message = dplyr::case_when(
          is.na(odp_longueur) == TRUE ~
            "Longueur manquante",
          odp_longueur <= fit_Lon + se.fit_Lon &
            odp_longueur >= fit_Lon - se.fit_Lon ~
            "Longueur normale",
          odp_longueur > fit_Lon + se.fit_Lon |
            odp_longueur < fit_Lon - se.fit_Lon ~
            "Longueur anormale"
        ),
        Lon_type = dplyr::case_when(
          is.na(odp_longueur) == TRUE ~ "alerte",
          odp_longueur <= fit_Lon + se.fit_Lon &
            odp_longueur >= fit_Lon - se.fit_Lon ~
            "",
          odp_longueur > fit_Lon + se.fit_Lon |
            odp_longueur < fit_Lon - se.fit_Lon ~
            "alerte",
          TRUE ~ ""
        )
      )

    if (plot == TRUE) {
      g_smooth <- ggplot2::ggplot(data_Lon, aes(x = odp_largeur_lame_eau,
                                                y = odp_longueur)) +
        ggplot2::geom_point(aes(colour = Lon_type)) +
        ggplot2::geom_line(aes(y = interval_max_Lon),
                           linetype = "dashed",
                           size = 1,
                           color = "#4ECBFC") +
        ggplot2::geom_line(aes(y = fit_Lon),
                           size = 1,
                           color = "#F52940") +
        ggplot2::geom_line(aes(y = interval_min_Lon),
                           linetype = "dashed",
                           size = 1,
                           color = "#4ECBFC") +
        ggrepel::geom_label_repel(aes(label = ifelse(ecart_Lon > 0,
                                                     as.character(ecart_Lon)
                                                     , '')),
                                  box.padding   = 0.35,
                                  point.padding = 0.5,
                                  segment.color = 'grey50') +
        ggplot2::ggtitle(paste0(
          "Longueur en fonction Largeur (label = ecart interval confiance, ",
          percent, "%)")) +
        ggplot2::labs(x = "Largeur", y = "Longeur", color = "Types") +
        ggplot2::scale_color_manual(values = pal_alerte)

      g2 <- data_Lon %>%
        dplyr::filter(ecart_Lon > 0) %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = factor(0), y = ecart_Lon)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point() +
        ggplot2::labs(x = "", y = "Longueur opération") +
        ggplot2::ggtitle("Ecart à l'intervalle de confiance")
    }

    commentaire <- paste0(
      "alerte pour ecart de Longeur > a ", percent, "%")

  } else {

    # Identify outliers according to an interval
    if (method == 1) {
      min_value_Lon <- min
      max_value_Lon <- max

      # Identify outliers with IQR criterion
    } else if (method == 2) {
      data_Lon <- data %>%
        dplyr::select(ope_id, odp_longueur, enh_libelle_sandre) %>%
        unique()

      min_value_Lon <- quantile(
        data_Lon$odp_longueur, probs = 0.25, na.rm = TRUE) -
        1.5*IQR(data_Lon$odp_longueur, na.rm = TRUE)
      max_value_Lon <- quantile(
        data_Lon$odp_longueur, probs = 0.75, na.rm = TRUE) +
        1.5*IQR(data_Lon$odp_longueur, na.rm = TRUE)

      if(is.null(percent) == FALSE) {
        min_value_Lon <- min_value_Lon - min_value_Lon * percent / 100
        max_value_Lon <- max_value_Lon + max_value_Lon * percent / 100
      }

      # Identify outliers with percentile method with interval of confidence
    } else if (method == 3) {
      num_lower <- ((100 - confidence.interval) / 2) / 100
      num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
      min_value_Lon <- stats::quantile(
        data$odp_longueur, num_lower, na.rm = TRUE)
      max_value_Lon <- stats::quantile(
        data$odp_longueur, num_upper, na.rm = TRUE)
    } else {
      stop("method inconnue")
    }
    commentaire <- paste0("Alertes en dehors de l'intervale [",
                          min_value_Lon, "; ", max_value_Lon, "]")
    print(commentaire)

    # Compute identification of alertes according to the method chosen
    data_Lon <- data %>%
      dplyr::select(ope_id, odp_longueur, enh_libelle_sandre,
                    odp_largeur_lame_eau) %>%
      unique() %>%
      dplyr::mutate(
        Lon_message = dplyr::case_when(
          is.na(odp_longueur) == TRUE ~ "Longueur manquante",
          min_value_Lon <= odp_longueur & odp_longueur <= max_value_Lon ~
            "Longueur normale",
          min_value_Lon > odp_longueur | odp_longueur > max_value_Lon ~
            "Longueur anormale"
        ),
        Lon_type = dplyr::case_when(
          is.na(odp_longueur) == TRUE ~ "alerte",
          min_value_Lon <= odp_longueur & odp_longueur <= max_value_Lon ~ "",
          min_value_Lon > odp_longueur | odp_longueur > max_value_Lon ~
            "alerte",
          TRUE ~ ""
        )
      )

    g_lim <- ggplot2::ggplot(data_Lon, aes(x = odp_largeur_lame_eau,
                                           y = odp_longueur)) +
      ggplot2::geom_point(aes(colour = Lon_type)) +
      ggplot2::geom_hline(yintercept = max_value_Lon,
                          linetype = "dashed",
                          size = 1,
                          color = "#4ECBFC") +
      ggplot2::geom_hline(yintercept = min_value_Lon,
                          linetype = "dashed",
                          size = 1,
                          color = "#4ECBFC") +
      ggplot2::ggtitle("Longueur en fonction Largeur") +
      ggplot2::labs(x = "Largeur", y = "Longeur", color = "Types") +
      ggplot2::scale_color_manual(values = pal_alerte)
  }

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_Lon %>%
      dplyr::group_by(ope_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = odp_longueur)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = Lon_type)) +
      ggplot2::labs(x = "", y = "Longueur opération", color = "alerte") +
      ggplot2::scale_color_manual(values = pal_alerte)

    g3 <- data_Lon %>%
      dplyr::group_by(ope_id) %>%
      ggplot2::ggplot(aes(x = as.factor(enh_libelle_sandre), y = odp_longueur)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = Lon_type)) +
      coord_flip() +
      ggplot2::labs(x = "Nom cours eau", y = "Longueur opération",
                    color = "alerte") +
      ggplot2::scale_color_manual(values = pal_alerte)
  }


  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_Lon) %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(Lon_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = odp_longueur), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des valeurs de Longueur en alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Longueur")
  }

  # Create statistics
  if (stat == TRUE) {
    Lon_stat <- data_Lon %>%
      plyr::summarise(
        na = round(
          mean(Lon_message == "Longueur manquante") * 100,
          digits = 2),
        alerte = round(
          mean(Lon_type == "alerte") * 100, digits = 2),
        Lon.anormale = round(
          mean(Lon_message == "Longueur anormale")* 100,
          digits = 2),
        Lon.normale = round(
          mean(Lon_message == "Longueur normale") * 100,
          digits = 2))

    Lon_nb <- data_Lon %>% #number
      plyr::summarise(
        na = sum(Lon_message == "Longueur manquante"),
        alerte = sum(Lon_type == "alerte"),
        Lon.anormale = sum(Lon_message == "Longueur anormale"),
        Lon.normale = sum(Lon_message == "Longueur normale"))

    # create tables
    table_stat <- make_tab(x = Lon_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Longueur (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = Lon_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Longueur (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      if (method == 4) {
        print(g_smooth)
        print(g2) # boxplot ecart
      } else if (method == 1 | method == 2 | method == 3) {
        print(g_lim)
      }
      print(g) # boxplot
      print(g3) # boxplot par cours eau
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

  # Plot smooth
  if(method == 4){
    print(g_smooth)
    print(g2) # boxplot ecart
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
  if (plot == TRUE) {
    print(g)
    grid::grid.newpage()
    print(g3)
    if (method == 1 | method == 2 | method == 3) {
      print(g_lim)
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
    utils::write.csv(data_Lon, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_Lon)
}
