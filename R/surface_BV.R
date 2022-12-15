# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de surface de bassin versant
#' @description Quatre methodes disponibles afin de tester la coherence des valeurs surface de bassin versant.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "pop_surfBV"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Quatre methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers. Si `method = 4`, une courbe de tendance entre la surface de bassin versant et la distance a la source est tracee et les outliers sont identifie selon un pourcentage d'ecart a cette tendance.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Pourcentage d'ecart a la tendance a prendre en compte si `method = 4` et pourcentage d'ecart aux valeurs seuils si `method = 2`.
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @seealso Pour verifier la que les valeurs de surface de bassin versant augmente bien avec l'augmentation de la distance a la source : \code{\link{test_pop_DS}}.
#'
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_surfBv <- test_pop_surfBV(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 4,
#' percent = 100)

#' @importFrom dplyr select inner_join mutate case_when group_by filter
#' @importFrom ggplot2 ggplot geom_point scale_color_manual geom_line ggtitle labs geom_boxplot geom_sf
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom stats quantile
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis

test_pop_surfBV <- function(data,
                            plot = TRUE,
                            stat = TRUE,
                            min = 100,
                            max = 2000,
                            dir = getwd(),
                            file.name = "pop_surfBV",
                            map = TRUE,
                            csv = TRUE,
                            method = 2,
                            confidence.interval = 95,
                            color.alerte = pal_alerte,
                            map.contour = IDF_contour,
                            map.cours.eau = cours_eau,
                            pdf = TRUE,
                            percent = 100) {

  # Autre methode en tracant relation avec la distance a la source
  if (method == 4){
    # select variables of interrest
    df <- data %>%
      dplyr::select(pop_id,
                    pop_surface_bassin_versant_amont,
                    pop_distance_source,
                    enh_libelle_sandre) %>%
      unique()
    # run loess model
    df.lo <- loess(pop_surface_bassin_versant_amont ~ pop_distance_source, df)
    # Get predictions ==> fit and se.fit vectors
    pred <- predict(df.lo, se = TRUE)
    # create a data.frame from those
    df2 <- data.frame(dist_source = df$pop_distance_source,
                      fit_surfBV = pred$fit,
                      #se.fit = pred$se.fit * qt(0.95 / 2 + .5, pred$df),
                      se.fit_surfBV = pred$fit * percent / 100,
                      interval_max_surfBV = pred$fit + pred$fit * percent / 100,
                      interval_min_surfBV = pred$fit - pred$fit * percent / 100,
                      pop_id = df$pop_id)


    # make the flag
    outerpoints <- +(df$pop_surface_bassin_versant_amont >
                       df2$fit_surfBV + df2$se.fit_surfBV |
                       df$pop_surface_bassin_versant_amont <
                       df2$fit_surfBV - df2$se.fit_surfBV)

    #add flag to original data frame
    df$outer_surfBV <- outerpoints
    df <- df %>%
      dplyr::inner_join(df2, by = "pop_id") %>%
      dplyr::mutate(
        ecart_surfBV = dplyr::case_when(
          pop_surface_bassin_versant_amont > fit_surfBV + se.fit_surfBV ~
            round(pop_surface_bassin_versant_amont - (fit_surfBV +
                                                        se.fit_surfBV)),
          pop_surface_bassin_versant_amont < fit_surfBV - se.fit_surfBV ~
            round((fit_surfBV - se.fit_surfBV) -
                    pop_surface_bassin_versant_amont),
          TRUE ~ 0
        )
      ) %>%
      dplyr::select(pop_id, pop_distance_source,
                    pop_surface_bassin_versant_amont,
                    fit_surfBV, se.fit_surfBV,
                    outer_surfBV, ecart_surfBV, enh_libelle_sandre,
                    interval_max_surfBV, interval_min_surfBV)

    data_surfBV <- df %>%
      dplyr::mutate(
        surfBV_message = dplyr::case_when(
          is.na(pop_surface_bassin_versant_amont) == TRUE ~
            "Surface bassin versant manquante",
          outer_surfBV == 0 ~ "Surface bassin versant normale",
          outer_surfBV == 1 ~ "Surface bassin versant anormale"
        ),
        surfBV_type = dplyr::case_when(
          is.na(pop_distance_source) == TRUE ~ "alerte",
          outer_surfBV == 0 ~ "",
          outer_surfBV == 1 ~ "alerte",
          TRUE ~ ""
        )
      )
    if (plot == TRUE) {
      g_smooth <- ggplot2::ggplot(data_surfBV, aes(pop_distance_source,
                                                   pop_surface_bassin_versant_amont)) +
        ggplot2::geom_point(aes(colour = surfBV_type)) +
        ggplot2::scale_color_manual(values = pal_alerte) +
        ggplot2::geom_line(aes(y = interval_max_surfBV),
                           linetype = "dashed",
                           size = 1,
                           color = "#4ECBFC") +
        ggplot2::geom_line(aes(y = fit_surfBV),
                           size = 1,
                           color = "#F52940") +
        ggplot2::geom_line(aes(y = interval_min_surfBV),
                           linetype = "dashed",
                           size = 1,
                           color = "#4ECBFC") +
        #ggplot2::geom_smooth() +
        ggrepel::geom_label_repel(aes(label = ifelse(ecart_surfBV > 0,
                                                     as.character(ecart_surfBV)
                                                     , '')),
                                  box.padding   = 0.35,
                                  point.padding = 0.5,
                                  segment.color = 'grey50') +
        ggplot2::ggtitle(paste0(
          "Surface BV en fonction distance source
        (label = ecart interval confiance, ",
          percent, "%)")) +
        ggplot2::labs(x = "Distance source", y = "Surface BV", color = "Types")

    }
    commentaire <- paste0(
      "alerte pour ecart de Surface BV > a ", percent, "%")
  } else {

    # Identify outliers according to an interval
    if (method == 1) {
      min_value_surfBV <- min
      max_value_surfBV <- max

      # Identify outliers with IQR criterion
    } else if (method == 2) {
      data_surfBV <- data %>%
        dplyr::select(pop_id,
                      pop_surface_bassin_versant_amont,
                      enh_libelle_sandre) %>%
        unique()

      min_value_surfBV <- quantile(
        data_surfBV$pop_surface_bassin_versant_amont,
        probs = 0.25, na.rm = TRUE) -
        1.5*IQR(data_surfBV$pop_surface_bassin_versant_amont, na.rm = TRUE)
      max_value_surfBV <- quantile(
        data_surfBV$pop_surface_bassin_versant_amont,
        probs = 0.75, na.rm = TRUE) +
        1.5*IQR(data_surfBV$pop_surface_bassin_versant_amont, na.rm = TRUE)

      if(is.null(percent) == FALSE) {
        min_value_surfBV <- min_value_surfBV - min_value_surfBV * percent / 100
        max_value_surfBV <- max_value_surfBV + max_value_surfBV * percent / 100
      }

      # Identify outliers with percentile method with interval of confidence
    } else if (method == 3) {
      num_lower <- ((100 - confidence.interval) / 2) / 100
      num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
      min_value_surfBV <- stats::quantile(
        data$pop_surface_bassin_versant_amont, num_lower, na.rm = TRUE)
      max_value_surfBV <- stats::quantile(
        data$pop_surface_bassin_versant_amont, num_upper, na.rm = TRUE)
    } else {
      stop("method inconnue")
    }
    commentaire <- paste0("Alertes en dehors de l'intervale [",
                          min_value_surfBV, "; ", max_value_surfBV, "]")
    print(commentaire)
    # Compute identification of alertes according to the method chosen
    data_surfBV <- data %>%
      dplyr::select(pop_id,
                    pop_surface_bassin_versant_amont,
                    enh_libelle_sandre) %>%
      unique() %>%
      dplyr::mutate(
        surfBV_message = dplyr::case_when(
          is.na(pop_surface_bassin_versant_amont) == TRUE ~
            "Surface bassin versant manquante",
          min_value_surfBV <= pop_surface_bassin_versant_amont &
            pop_surface_bassin_versant_amont <= max_value_surfBV ~
            "Surface bassin versant normale",
          min_value_surfBV > pop_surface_bassin_versant_amont |
            pop_surface_bassin_versant_amont > max_value_surfBV ~
            "Surface bassin versant anormale"
        ),
        surfBV_type = dplyr::case_when(
          is.na(pop_surface_bassin_versant_amont) == TRUE ~ "alerte",
          min_value_surfBV <= pop_surface_bassin_versant_amont &
            pop_surface_bassin_versant_amont <= max_value_surfBV ~ "",
          min_value_surfBV > pop_surface_bassin_versant_amont |
            pop_surface_bassin_versant_amont > max_value_surfBV ~ "alerte",
          TRUE ~ ""
        )
      )
  }


  # Create the boxplot
  if (plot == TRUE) {
    g <- data_surfBV %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_surface_bassin_versant_amont)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = surfBV_type)) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      ggplot2::labs(x = "", y = "Surface BV", color = "alerte")

    g2 <- data_surfBV %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = as.factor(enh_libelle_sandre),
                          y = pop_surface_bassin_versant_amont)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = surfBV_type)) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      coord_flip() +
      ggplot2::labs(x = "Nom cours eau", y = "Surface BV", color = "alerte")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_surfBV) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(surfBV_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_surface_bassin_versant_amont),
                          size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des operation classees alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    color = "Surface Bassin versant amont")
  }

  # Create statistics
  if (stat == TRUE) {
    surfBV_stat <- data_surfBV %>% # Percent
      plyr::summarise(
        na = round(
          mean(surfBV_message == "Surface bassin versant manquante") * 100,
          digits = 2),
        alerte = round(
          mean(surfBV_type == "alerte") * 100, digits = 2),
        surfBV.anormale = round(
          mean(surfBV_message == "Surface bassin versant anormale") * 100,
          digits = 2),
        surfBV.normale = round(
          mean(surfBV_message == "Surface bassin versant normale") * 100,
          digits = 2))

    surfBV_nb <- data_surfBV %>% # Number
      plyr::summarise(
        na = sum(
          surfBV_message == "Surface bassin versant manquante"),
        alerte = sum(
          surfBV_type == "alerte"),
        surfBV.anormale = sum(
          surfBV_message == "Surface bassin versant anormale"),
        surfBV.normale = sum(
          surfBV_message == "Surface bassin versant normale"))

    # create tables
    table_stat <- make_tab(x = surfBV_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport surface bassin verssant (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = surfBV_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport surface bassin versant (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g) # boxplot
      print(g2)
      if (method == 4) {
        print(g_smooth)
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
    grid::grid.newpage()
    grid::grid.draw(table_stat)
    grid::grid.newpage()
    grid::grid.draw(table_nb)
  } else {
    print("no statistic draw ==> write stat = TRUE to see it")
  }
  # Plot the boxplot
  if (plot == TRUE) {
    print(g)
    grid::grid.newpage()
    print(g2)
    if (method == 4) { # geom_smooth
      print(g_smooth)
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
    utils::write.csv(data_surfBV, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_surfBV)
}
