# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs d'altitude
#' @description Trois methodes disponibles afin de tester la coherence des valeurs d'altitude.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "tempJanv"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Trois methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Pourcentage d'ecart aux valeurs seuils si `method = 2`.

#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Pour l'Ile de France, une autre fonction permet de comparer aves des valeurs d'altitudes locales : \code{\link{test_pop_altIDF}}. Il est egalement possible de verifier que les valeurs d'altitude diminue bien avec la distance a la source : \code{\link{test_pop_DS}}.
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @examples
#' result_alt <- test_pop_alt(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 2)
test_pop_alt <- function(data,
                         plot = TRUE,
                         stat = TRUE,
                         min = 10,
                         max = 200,
                         dir = getwd(),
                         file.name = "pop_alt",
                         map = TRUE,
                         csv = TRUE,
                         method = 3,
                         confidence.interval = 95,
                         color.alerte = pal_alerte,
                         map.contour = IDF_contour,
                         map.cours.eau = cours_eau,
                         pdf = TRUE,
                         percent = NULL) {

  # Identify outliers according to an interval
  if (method == 1) {
    min_value_alt <- min
    max_value_alt <- max

    # Identify outliers with IQR criterion
  } else if (method == 2) {
    data_alt <- data %>%
      dplyr::select(pop_id, pop_altitude) %>%
      unique()
    min_value_alt <- quantile(
      data_alt$pop_altitude, probs = 0.25, na.rm = TRUE) -
      1.5*IQR(data_alt$pop_altitude, na.rm = TRUE)
    max_value_alt <- quantile(
      data_alt$pop_altitude, probs = 0.75, na.rm = TRUE) +
      1.5*IQR(data_alt$pop_altitude, na.rm = TRUE)

    if(is.null(percent) == FALSE) {
      min_value_alt <- min_value_alt - min_value_alt * percent / 100
      max_value_alt <- max_value_alt + max_value_alt * percent / 100
    }

    # Identify outliers with percentile method with interval of confidence
  } else if (method == 3) {
    num_lower <- ((100 - confidence.interval) / 2) / 100
    num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
    min_value_alt <- stats::quantile(data$pop_altitude, num_lower, na.rm = TRUE)
    max_value_alt <- stats::quantile(data$pop_altitude, num_upper, na.rm = TRUE)
  } else {
    stop("method inconnue")
  }

  commentaire <- paste0("Alertes en dehors de l'intervale [",
                        min_value_alt, "; ", max_value_alt, "]")
  print(commentaire)

  # Compute identification of alertes according to the method chosen
  data_alt <- data %>%
    dplyr::select(pop_id, pop_altitude) %>%
    unique() %>%
    dplyr::mutate(
      alt_message = dplyr::case_when(
        is.na(pop_altitude) == TRUE ~ "Altitude manquante",
        min_value_alt <= pop_altitude & pop_altitude <= max_value_alt ~
          "Altitude normale",
        min_value_alt > pop_altitude | pop_altitude > max_value_alt ~
          "Altitude anormale"
      ),
      alt_type = dplyr::case_when(
        is.na(pop_altitude) == TRUE ~ "alerte",
        min_value_alt <= pop_altitude & pop_altitude <= max_value_alt ~ "",
        min_value_alt > pop_altitude | pop_altitude > max_value_alt ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_alt %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_altitude)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = alt_type)) +
      ggplot2::scale_color_manual(values = pal_alerte)
  }

  # Create the map
  if (plot == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_alt) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(alt_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = IDF_contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = cours_eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_altitude), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des points de prelevements classes alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Altitude")
  }

  # Create statistics
  if (stat == TRUE) {
    alt_stat <- data_alt %>% # percent
      plyr::summarise(
        na = round(
          mean(alt_message == "Altitude manquante") * 100,
          digits = 2),
        alerte = round(
          mean(alt_type == "alerte") * 100, digits = 2),
        alt.anormale = round(
          mean(alt_message == "Altitude anormale")* 100,
          digits = 2),
        alt.normale = round(
          mean(alt_message == "Altitude normale") * 100,
          digits = 2))

    alt_nb <- data_alt %>% # Number
      plyr::summarise(
        na = sum(
          alt_message == "Altitude manquante"),
        alerte = sum(
          alt_type == "alerte"),
        alt.anormale = sum(
          alt_message == "Altitude anormale"),
        alt.normale = sum(
          alt_message == "Altitude normale"))

    # create tables
    table_stat <- make_tab(x = alt_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Altitude (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = alt_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Altitude (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g) # boxplot
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
    grid::grid.draw(table_stat)
    grid::grid.newpage()
    grid::grid.draw(table_nb)
  } else {
    print("no statistic draw ==> write stat = TRUE to see it")
  }
  # Plot the boxplot
  if (plot == TRUE) {
    print(g)
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
    utils::write.csv(data_alt, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_alt)
}

# ===========================

#' Coherence des valeurs d'altitude par rapport a un modele.
#' @description Deux methodes et deux echelles disponibles afin de tester la coherence des valeurs d'altitude par rapport a un modele d'altitude d'IDF.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param modele Modele d'altitude locale a utiliser. Par defaut, `modele = altitude_IDF` un modele uniquement pour l'Ile de France.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "pop_altIDF"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Une methode pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers.
#' @param echelle Argument character. Prend deux valeurs possibles `"A"/"B"`. Si `echelle = "A"`, identifie les outliers a l'echelle des masses d'eau. Si `echelle = "B"`, identifie les outliers a l'echelle des points de prelevements.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).

#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso En dehors de l'Ile de France ou sur des stations differentes, une fonction permettant de verifier la coherence des valeurs d'altitude : \code{\link{test_pop_alt}}.
#'
#' @examples
#' test_pop_altIDF(
#' data = exemple_IDF,
#' modele = altitude_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' echelle = "B")
test_pop_altIDF <- function(data,
                            modele = altitude_IDF,
                            plot = TRUE,
                            stat = TRUE,
                            dir = getwd(),
                            file.name = "pop_altIDF",
                            map = TRUE,
                            csv = TRUE,
                            method = 1,
                            echelle = "A",
                            color.alerte = pal_alerte,
                            map.contour = IDF_contour,
                            map.cours.eau = cours_eau,
                            pdf = TRUE) {
  # warnings
  if (echelle != "A" & echelle != "B") {
    stop("echelle inconnue")
  }
  if (method != 1) {
    stop("method inconnue")
  }

  # Create dataframe
  modele$sta_com_code_insee <- as.character(
    modele$sta_com_code_insee)
  join_alt <- left_join(data, modele)
  df1 <- join_alt %>%
    dplyr::group_by(pop_id) %>%
    dplyr::group_split() %>%
    purrr::map_df(
      .f = function(df) {
        if (echelle == "A") {
          # Identify outliers according to an interval
          if (method == 1) {
            df <- df %>%
              mutate(min_value_altIDF = alt_min,
                     max_value_altIDF = alt_max)
            return(df)
          }
          # Identify outliers with IQR criterion
        } else if (echelle == "B") {
          if (method == 1) {
            df <- df %>%
              mutate(min_value_altIDF = alt_pop_min,
                     max_value_altIDF = alt_pop_max)
            return(df)
          }
        }
      }
    )


  # Compute identification of alertes according to the method chosen
  data_altIDF <- df1 %>%
    dplyr::select(pop_id, pop_altitude, min_value_altIDF, max_value_altIDF) %>%
    unique() %>%
    dplyr::mutate(
      altIDF_message = dplyr::case_when(
        is.na(pop_altitude) == TRUE ~ "Altitude manquante",
        min_value_altIDF <= pop_altitude & pop_altitude <= max_value_altIDF ~
          "Altitude normale",
        min_value_altIDF > pop_altitude | pop_altitude > max_value_altIDF ~
          "Altitude anormale"
      ),
      altIDF_type = dplyr::case_when(
        is.na(pop_altitude) == TRUE ~ "alerte",
        min_value_altIDF <= pop_altitude &
          pop_altitude <= max_value_altIDF ~ "",
        min_value_altIDF > pop_altitude |
          pop_altitude > max_value_altIDF ~ "alerte",
        TRUE ~ ""
      ),
      altIDF_ecart = dplyr::case_when(
        is.na(pop_altitude) == TRUE ~ NA_real_,
        min_value_altIDF <= pop_altitude & pop_altitude <= max_value_altIDF ~
          0,
        min_value_altIDF > pop_altitude ~ min_value_altIDF - pop_altitude,
        pop_altitude > max_value_altIDF ~ pop_altitude - max_value_altIDF
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_altIDF %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_altitude)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = altIDF_type)) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      ggplot2::labs(x = "", y = "Altitude", color = "Type")

    # Create the boxplot for each POP
    # g2 <- data_altIDF %>%
    #   dplyr::group_by(pop_id) %>%
    #   ggplot2::ggplot(aes(x = as.factor(pop_id), y = pop_altitude)) +
    #   ggplot2::geom_boxplot(fill = "grey60") +
    #   ggplot2::geom_point(aes(color = altIDF_type)) +
    #   ggplot2::coord_flip() +
    #   ggplot2::scale_color_manual(values = pal_alerte) +
    #   ggplot2::labs(x = "Points de prelevement", y = "Altitude", color = "Type")

    # Boxplot des ecarts
    if (any(data_altIDF$altIDF_ecart > 0) == TRUE) {
      g3 <- data_altIDF %>%
        dplyr::group_by(pop_id) %>%
        dplyr::filter(altIDF_ecart > 0) %>%
        ggplot2::ggplot(aes(x = factor(0), y = altIDF_ecart)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::labs(x = "", y = "Valeurs des ecarts d'altitude")
    }
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_altIDF) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(altIDF_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_altitude), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des points de prelevements classes alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Altitude")
  }

  # Create statistics
  if (stat == TRUE) {
    altIDF_stat <- data_altIDF %>% # percent
      plyr::summarise(
        na = round(
          mean(altIDF_message == "Altitude manquante") * 100,
          digits = 2),
        alerte = round(
          mean(altIDF_type == "alerte") * 100, digits = 2),
        alt.anormale = round(
          mean(altIDF_message == "Altitude anormale")* 100,
          digits = 2),
        alt.normale = round(
          mean(altIDF_message == "Altitude normale") * 100,
          digits = 2))

    altIDF_nb <- data_altIDF %>% # Number
      plyr::summarise(
        na = sum(
          altIDF_message == "Altitude manquante"),
        alerte = sum(
          altIDF_type == "alerte"),
        alt.anormale = sum(
          altIDF_message == "Altitude anormale"),
        alt.normale = sum(
          altIDF_message == "Altitude normale"))

    # create tables
    commentaire <- paste0(
      "Alertes en dehors de l'interval de chaque masse d'eau/POP")
    table_stat <- make_tab(x = altIDF_stat,
                           size.tab = 12,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Altitude (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = altIDF_nb,
                         size.tab = 12,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Altitude (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g) # boxplot
      # print(g2) # boxplot par POP
      if (any(data_altIDF$altIDF_ecart > 0) == TRUE) {
        print(g3) # boxplot des ecarts
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
    grid::grid.draw(table_stat)
    grid::grid.newpage()
    grid::grid.draw(table_nb)
  } else {
    print("no statistic draw ==> write stat = TRUE to see it")
  }
  # Plot the boxplot
  if (plot == TRUE) {
    print(g)
    # grid::grid.newpage()
    # print(g2)
    grid::grid.newpage()
    if (any(data_altIDF$altIDF_ecart > 0) == TRUE) {
      print(g3) # boxplot des ecarts
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
    utils::write.csv(data_altIDF, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_altIDF)
}
