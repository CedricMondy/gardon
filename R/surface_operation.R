# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de surface de l'operation
#' @description Trois methodes disponibles afin de tester la coherence des valeurs de surface de l'operation.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_surf"`.
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
#' @seealso Pour verifier le calcul de la surface de l'operation : \code{\link{test_ope_CalSurf}}.
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_surf <- test_ope_surf(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 2)

#' @importFrom dplyr select mutate case_when group_by inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual geom_sf ggtitle labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom stats quantile
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis

test_ope_surf <- function(data,
                          plot = TRUE,
                          stat = TRUE,
                          min = 100,
                          max = 2000,
                          dir = getwd(),
                          file.name = "ope_surf",
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
    min_value_surf <- min
    max_value_surf <- max

    # Identify outliers with IQR criterion
  } else if (method == 2) {
    data_surf <- data %>%
      dplyr::select(ope_id, ope_surface_calculee) %>%
      unique()

    min_value_surf <- quantile(
      data_surf$ope_surface_calculee, probs = 0.25, na.rm = TRUE) -
      1.5*IQR(data_surf$ope_surface_calculee, na.rm = TRUE)
    max_value_surf <- quantile(
      data_surf$ope_surface_calculee, probs = 0.75, na.rm = TRUE) +
      1.5*IQR(data_surf$ope_surface_calculee, na.rm = TRUE)

    if(is.null(percent) == FALSE) {
      min_value_surf <- min_value_surf - min_value_surf * percent / 100
      max_value_surf <- max_value_surf + max_value_surf * percent / 100
    }

    # Identify outliers with percentile method with interval of confidence of 95%
  } else if (method == 3) {
    num_lower <- ((100 - confidence.interval) / 2) / 100
    num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
    min_value_surf <- stats::quantile(
      data$ope_surface_calculee, num_lower, na.rm = TRUE)
    max_value_surf <- stats::quantile(
      data$ope_surface_calculee, num_upper, na.rm = TRUE)
  } else {
    stop("method inconnue")
  }

  commentaire <- paste0("Alertes en dehors de l'intervale [",
                        min_value_surf, "; ", max_value_surf, "]")
  print(commentaire)

  # Compute identification of alertes according to the method chosen
  data_surf <- data %>%
    dplyr::select(ope_id, ope_surface_calculee) %>%
    unique() %>%
    dplyr::mutate(
      surf_message = dplyr::case_when(
        is.na(ope_surface_calculee) == TRUE ~ "Surface manquante",
        min_value_surf <= ope_surface_calculee &
          ope_surface_calculee <= max_value_surf ~ "Surface normale",
        min_value_surf > ope_surface_calculee |
          ope_surface_calculee > max_value_surf ~ "Surface anormale"
      ),
      surf_type = dplyr::case_when(
        is.na(ope_surface_calculee) == TRUE ~ "alerte",
        min_value_surf <= ope_surface_calculee &
          ope_surface_calculee <= max_value_surf ~ "",
        min_value_surf > ope_surface_calculee |
          ope_surface_calculee > max_value_surf ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_surf %>%
      dplyr::group_by(ope_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = ope_surface_calculee)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = surf_type)) +
      ggplot2::scale_color_manual(values = pal_alerte)
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_surf) %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(surf_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = ope_surface_calculee), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des operation classees alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Surface calculee")
  }

  # Create statistics
  if (stat == TRUE) {
    surf_stat <- data_surf %>% #percent
      plyr::summarise(
        na = round(
          mean(surf_message == "Surface manquante") * 100,
          digits = 2),
        alerte = round(
          mean(surf_type == "alerte") * 100, digits = 2),
        surf.anormale = round(
          mean(surf_message == "Surface anormale")* 100,
          digits = 2),
        surf.normale = round(
          mean(surf_message == "Surface normale") * 100,
          digits = 2))

    surf_nb <- data_surf %>% # Number
      plyr::summarise(
        na = sum(
          surf_message == "Surface manquante"),
        alerte = sum(
          surf_type == "alerte"),
        surf.anormale = sum(
          surf_message == "Surface anormale"),
        surf.normale = sum(
          surf_message == "Surface normale"))

    # create tables
    table_stat <- make_tab(x = surf_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport surface calculee (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = surf_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport surface calculee (nb)",
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
    utils::write.csv(data_surf, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_surf)
}

# ===========================

#' Verification du calcul de surface de l'operation
#' @description Test qui permet de verifier la conformite du calcul de la surface en fonction du protocole d'echantillonnage.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_CalSurf"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).

#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Pour verifier la coherence des valeurs de surface de l'operation : \code{\link{test_ope_surf}}.
#'
#' @examples
#' result_CalSurf <- test_ope_CalSurf(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE)

#' @importFrom dplyr filter select mutate case_when group_by inner_join
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual labs geom_sf ggtitle
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis

test_ope_CalSurf <- function(data,
                             plot = TRUE,
                             stat = TRUE,
                             dir = getwd(),
                             file.name = "ope_CalSurf",
                             map = TRUE,
                             csv = TRUE,
                             color_alerte = pal_alerte,
                             map.contour = IDF_contour,
                             map.cours.eau = cours_eau,
                             pdf = TRUE) {

  # Compute identification of alertes according to the protocole
  data_CalSurf <- data %>%
    dplyr::filter(is.na(grp_tgp_id) == TRUE | grp_tgp_id == 1) %>%
    # on exclue les points complementaires
    dplyr::select(ope_id,
                  ope_surface_calculee,
                  odp_largeur_lame_eau,
                  odp_longueur,
                  ope_pro_id,
                  grp_tgp_id,
                  grp_nombre) %>%
    unique() %>%
    dplyr::mutate(
      surf_cal = dplyr::case_when(
        ope_pro_id == 1 ~ odp_largeur_lame_eau * odp_longueur,
        ope_pro_id == 2 ~ (12.5 * grp_nombre)
      ),
      CalSurf_message = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "Protocole non renseigne",
        ope_pro_id == 1 &
          (round(ope_surface_calculee, digits = 2) ==
             round(surf_cal, digits = 2)) ~
          "Surface normale pour une peche complete",
        ope_pro_id == 1 &
          (round(ope_surface_calculee, digits = 2) !=
             round(surf_cal, digits = 2)) ~
          "Surface anormale pour une peche complete",
        ope_pro_id == 2 &
          (round(ope_surface_calculee, digits = 2) ==
             round(surf_cal, digits = 2)) ~
          "Surface normale pour une peche partielle",
        ope_pro_id == 2 &
          (round(ope_surface_calculee, digits = 2) !=
             round(surf_cal, digits = 2)) ~
          "Surface anormale pour une peche partielle"
      ),
      CalSurf_type = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "alerte",
        ope_pro_id == 1 &
          (round(ope_surface_calculee, digits = 2) ==
             round(surf_cal, digits = 2)) ~ "",
        ope_pro_id == 1 &
          (round(ope_surface_calculee, digits = 2) !=
             round(surf_cal, digits = 2)) ~ "alerte",
        ope_pro_id == 2 &
          (round(ope_surface_calculee, digits = 2) ==
             round(surf_cal, digits = 2)) ~ "",
        ope_pro_id == 2 &
          (round(ope_surface_calculee, digits = 2) !=
             round(surf_cal, digits = 2)) ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_CalSurf %>%
      dplyr::group_by(ope_id) %>%
      ggplot2::ggplot(aes(x = as.factor(ope_pro_id), y = ope_surface_calculee)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = CalSurf_type)) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      ggplot2::labs(x = "Type de protocole", y = "Surface calculee",
                    color = "alerte")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_CalSurf) %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(CalSurf_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = IDF_contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = cours_eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = ope_surface_calculee), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des operation classees alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Surface calculee")
  }

  # Create statistics
  if (stat == TRUE) {
    CalSurf_stat <- data_CalSurf %>%
      plyr::summarise(
        na = round(
          mean(CalSurf_message == "Protocole non renseigne") * 100,
          digits = 2),
        alerte = round(
          mean(CalSurf_type == "alerte") * 100, digits = 2),
        surf.compl.anormale = round(
          mean(CalSurf_message == "Surface anormale pour une peche complete")
          * 100, digits = 2),
        surf.compl.normale = round(
          mean(CalSurf_message == "Surface normale pour une peche complete")
          * 100, digits = 2),
        surf.part.anormale = round(
          mean(CalSurf_message == "Surface anormale pour une peche partielle")
          * 100, digits = 2),
        surf.part.normale = round(
          mean(CalSurf_message == "Surface normale pour une peche partielle")
          * 100, digits = 2))

    CalSurf_nb <- data_CalSurf %>%
      plyr::summarise(
        na = sum(CalSurf_message == "Protocole non renseigne"),
        alerte = sum(CalSurf_type == "alerte"),
        surf.compl.anormale = sum(
          CalSurf_message == "Surface anormale pour une peche complete"),
        surf.compl.normale = sum(
          CalSurf_message == "Surface normale pour une peche complete"),
        surf.part.anormale = sum(
          CalSurf_message == "Surface anormale pour une peche partielle"),
        surf.part.normale = sum(
          CalSurf_message == "Surface normale pour une peche partielle"))

    # Create tables
    commentaire <- c("Alertes erreurs calcul surface en fonction protocole peche")

    table_stat <- make_tab(x = CalSurf_stat,
                           colours = c("#BF3111", "#BF3111", "#BF3111",
                                       "#5AB5BF", "#BF3111", "#5AB5BF"),
                           titre = "Rapport check Surface calculee (%)",
                           comment = commentaire,
                           size.tab = 7)
    table_nb <- make_tab(x = CalSurf_nb,
                         colours = c("#BF3111", "#BF3111", "#BF3111",
                                     "#5AB5BF", "#BF3111", "#5AB5BF"),
                         titre = "Rapport check Surface calculee (nb)",
                         comment = commentaire,
                         size.tab = 7)
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
    grid::grid.draw(table_stat) # percent
    grid::grid.newpage()
    grid::grid.draw(table_nb) # nb
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
    utils::write.csv(data_CalSurf, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_CalSurf)
}

# ===========================
