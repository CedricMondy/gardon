# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de temperature de janvier
#' @description Trois methodes disponibles afin de tester la coherence des valeurs de temperature de janvier.
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
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_tempJanv <- test_pop_tempJanv(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 2)

#' @importFrom dplyr select mutate case_when group_by inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual labs geom_sf ggtitle
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom viridis scale_color_viridis

test_pop_tempJanv <- function(data,
                              plot = TRUE,
                              stat = TRUE,
                              min = 0,
                              max = 20,
                              dir = getwd(),
                              file.name = "tempJanv",
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
    min_value_tempJanv <- min
    max_value_tempJanv <- max

    # Identify outliers with IQR criterion
  } else if (method == 2) {
    data_tempJanv <- data %>%
      dplyr::select(pop_id, pop_temperature_moyenne_janvier) %>%
      unique()
    min_value_tempJanv <- quantile(
      data_tempJanv$pop_temperature_moyenne_janvier,
      probs = 0.25, na.rm = TRUE) -
      (1.5*IQR(data_tempJanv$pop_temperature_moyenne_janvier, na.rm = TRUE))
    max_value_tempJanv <- quantile(
      data_tempJanv$pop_temperature_moyenne_janvier,
      probs = 0.75, na.rm = TRUE) +
      (1.5*IQR(data_tempJanv$pop_temperature_moyenne_janvier, na.rm = TRUE))
    if(is.null(percent) == FALSE) {
      min_value_tempJanv <- min_value_tempJanv -
        min_value_tempJanv * percent / 100
      max_value_tempJanv <- max_value_tempJanv +
        max_value_tempJanv * percent / 100
    }

    # Identify outliers with percentile method with interval of confidence
  } else if (method == 3) {
    num_lower <- ((100 - confidence.interval) / 2) / 100
    num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
    min_value_tempJanv <- stats::quantile(data$pop_temperature_moyenne_janvier,
                                          num_lower, na.rm = TRUE)
    max_value_tempJanv <- stats::quantile(data$pop_temperature_moyenne_janvier,
                                          num_upper, na.rm = TRUE)
  } else {
    stop("method inconnue")
  }
  commentaire <- paste0("Alertes en dehors de l'intervale [",
                        min_value_tempJanv, "; ", max_value_tempJanv, "]")
  print(commentaire)

  # Compute identification of alertes according to the method chosen
  data_tempJanv <- data %>%
    dplyr::select(pop_id, pop_temperature_moyenne_janvier) %>%
    unique() %>%
    dplyr::mutate(
      tempJanv_message = dplyr::case_when(
        is.na(pop_temperature_moyenne_janvier) == TRUE ~
          "Temperature janvier manquante",
        min_value_tempJanv <= pop_temperature_moyenne_janvier &
          pop_temperature_moyenne_janvier <= max_value_tempJanv ~
          "Temperature janvier normale",
        min_value_tempJanv > pop_temperature_moyenne_janvier |
          pop_temperature_moyenne_janvier > max_value_tempJanv ~
          "Temperature janvier anormale"
      ),
      tempJanv_type = dplyr::case_when(
        is.na(pop_temperature_moyenne_janvier) == TRUE ~ "alerte",
        min_value_tempJanv <= pop_temperature_moyenne_janvier &
          pop_temperature_moyenne_janvier <= max_value_tempJanv ~ "",
        min_value_tempJanv > pop_temperature_moyenne_janvier |
          pop_temperature_moyenne_janvier > max_value_tempJanv ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_tempJanv %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_temperature_moyenne_janvier)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = tempJanv_type)) +
      ggplot2::scale_color_manual(values = color.alerte) +
      ggplot2::labs(x = "", y = "Temperature janvier", color = "Types")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_tempJanv) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(tempJanv_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_temperature_moyenne_janvier), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des points de prelevements classes alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    color = "Temperature janvier")
  }

  # Create statistics
  if (stat == TRUE) {
    tempJanv_stat <- data_tempJanv %>% # percent
      plyr::summarise(
        na = round(mean(
          tempJanv_message == "Temperature janvier manquante") * 100,
          digits = 2),
        alerte = round(mean(
          tempJanv_type == "alerte") * 100,
          digits = 2),
        tempJanv.anormale = round(mean(
          tempJanv_message == "Temperature janvier anormale") * 100,
          digits = 2),
        tempJanv.normale = round(mean(
          tempJanv_message == "Temperature janvier normale") * 100,
          digits = 2))

    tempJanv_nb <- data_tempJanv %>% # Number
      plyr::summarise(
        na = sum(
          tempJanv_message == "Temperature janvier manquante"),
        alerte = sum(
          tempJanv_type == "alerte"),
        tempJanv.anormale = sum(
          tempJanv_message == "Temperature janvier anormale"),
        tempJanv.normale = sum(
          tempJanv_message == "Temperature janvier normale"))

    # create tables
    table_stat <- make_tab(x = tempJanv_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Temperature janvier (%)",
                           comment = commentaire,
                           size.titre = 10)
    table_nb <- make_tab(x = tempJanv_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Temperature janvier (nb)",
                         comment = commentaire,
                         size.titre = 10)
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
    if (stat == FALSE & map == FALSE & stat == FALSE) {
      grid::grid.newpage()
    }
    grDevices::dev.off()
  }
  # Plot statistics
  if (stat == TRUE) {
    print(grid::grid.draw(table_stat))
    grid::grid.newpage()
    print(grid::grid.draw(table_nb))
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
    utils::write.csv(data_tempJanv, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_tempJanv)
}

# ===========================

#' Coherence des valeurs de temperature de juillet
#' @description Trois methodes disponibles afin de tester la coherence des valeurs de temperature de juillet.
#'
#' @param data Donnees de sortie de la fonction `GetdataRegion()`. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = getwd()`).
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
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_tempJuil <- test_pop_tempJuil(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 2)

#' @importFrom dplyr select mutate case_when group_by inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual labs geom_sf ggtitle
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom viridis scale_color_viridis

test_pop_tempJuil <- function(data,
                              plot = TRUE,
                              stat = TRUE,
                              min = 0,
                              max = 20,
                              dir = getwd(),
                              file.name = "tempJuil",
                              map = TRUE,
                              csv = TRUE,
                              method = 3,
                              confidence.interval = 95,
                              map.contour = IDF_contour,
                              map.cours.eau = cours_eau,
                              pdf = TRUE,
                              percent = NULL) {

  # Identify outliers according to an interval
  if (method == 1) {
    min_value_tempJuil <- min
    max_value_tempJuil <- max

    # Identify outliers with IQR criterion
  } else if (method == 2) {
    data_tempJuil <- data %>%
      dplyr::select(pop_id, pop_temperature_moyenne_juillet) %>%
      unique()
    min_value_tempJuil <- quantile(
      data_tempJuil$pop_temperature_moyenne_juillet,
      probs = 0.25, na.rm = TRUE) -
      (1.5*IQR(data_tempJuil$pop_temperature_moyenne_juillet, na.rm = TRUE))
    max_value_tempJuil <- quantile(
      data_tempJuil$pop_temperature_moyenne_juillet,
      probs = 0.75, na.rm = TRUE) +
      (1.5*IQR(data_tempJuil$pop_temperature_moyenne_juillet, na.rm = TRUE))

    if(is.null(percent) == FALSE) {
      min_value_tempJuil <- min_value_tempJuil -
        min_value_tempJuil * percent / 100
      max_value_tempJuil <- max_value_tempJuil +
        max_value_tempJuil * percent / 100
    }

    # Identify outliers with percentile method with interval of confidence
  } else if (method == 3) {
    num_lower <- ((100 - confidence.interval) / 2) / 100
    num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
    min_value_tempJuil <- stats::quantile(data$pop_temperature_moyenne_juillet,
                                          num_lower, na.rm = TRUE)
    max_value_tempJuil <- stats::quantile(data$pop_temperature_moyenne_juillet,
                                          num_upper, na.rm = TRUE)
  } else {
    stop("method inconnue")
  }

  commentaire <- paste0("Alertes en dehors de l'intervale [",
                        min_value_tempJuil, "; ", max_value_tempJuil, "]")
  print(commentaire)

  # Compute identification of alertes according to the method chosen
  data_tempJuil <- data %>%
    dplyr::select(pop_id, pop_temperature_moyenne_juillet) %>%
    unique() %>%
    dplyr::mutate(
      tempJuil_message = dplyr::case_when(
        is.na(pop_temperature_moyenne_juillet) == TRUE ~
          "Temperature juillet manquante",
        min_value_tempJuil <= pop_temperature_moyenne_juillet &
          pop_temperature_moyenne_juillet <= max_value_tempJuil ~
          "Temperature juillet normale",
        min_value_tempJuil > pop_temperature_moyenne_juillet |
          pop_temperature_moyenne_juillet >
          max_value_tempJuil ~ "Temperature juillet anormale"
      ),
      tempJuil_type = dplyr::case_when(
        is.na(pop_temperature_moyenne_juillet) == TRUE ~ "alerte",
        min_value_tempJuil <= pop_temperature_moyenne_juillet &
          pop_temperature_moyenne_juillet <= max_value_tempJuil ~ "",
        min_value_tempJuil > pop_temperature_moyenne_juillet |
          pop_temperature_moyenne_juillet > max_value_tempJuil ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if(plot == TRUE) {
    g <- data_tempJuil %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_temperature_moyenne_juillet)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = tempJuil_type)) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      ggplot2::labs(x = "", y = "Temperature juillet", color = "Types")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_tempJuil) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(tempJuil_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_temperature_moyenne_juillet), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des points de prelevements classes alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    color = "Temperature juillet")
  }

  # Create statistics
  if (stat == TRUE) {
    tempJuil_stat <- data_tempJuil %>%
      plyr::summarise(
        na = round(mean(
          tempJuil_message == "Temperature juillet manquante") * 100,
          digits = 2),
        alerte = round(mean(
          tempJuil_type == "alerte") * 100, digits = 2),
        tempJuil.anormale = round(mean(
          tempJuil_message == "Temperature juillet anormale")* 100, digits = 2),
        tempJuil.normale = round(mean(
          tempJuil_message == "Temperature juillet normale") * 100, digits = 2))

    tempJuil_nb <- data_tempJuil %>% # Number
      plyr::summarise(
        na = sum(
          tempJuil_message == "Temperature juillet manquante"),
        alerte = sum(
          tempJuil_type == "alerte"),
        tempJuil.anormale = sum(
          tempJuil_message == "Temperature juillet anormale"),
        tempJuil.normale = sum(
          tempJuil_message == "Temperature juillet normale"))

    # create tables
    table_stat <- make_tab(x = tempJuil_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Temperature juillet (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = tempJuil_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Temperature juillet (nb)",
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
    utils::write.csv(data_tempJuil, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_tempJuil)
}

# ===========================
