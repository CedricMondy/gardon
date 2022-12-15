# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de distance a la source
#' @description Trois methodes disponibles afin de tester la coherence des valeurs de distance a la source.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}.Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "pop_dist"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Trois methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param color_alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Pourcentage d'ecart aux valeurs seuils si `method = 2`.

#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @details En ce qui concerne l'IQR criterion (`method = 2`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @seealso Pour verifier le lien entre les valeurs de distance a la source et celles d'altitude et de surface de bassin versant : \code{\link{test_pop_DS}}.
#'
#' @examples
#' result_dist <- test_pop_dist(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 2
#' )
#' @importFrom dplyr select mutate case_when group_by inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual labs geom_sf ggtitle
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom stats quantile
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis
test_pop_dist <- function(data,
                          plot = TRUE,
                          stat = TRUE,
                          min = 10,
                          max = 300,
                          dir = getwd(),
                          file.name = "pop_dist",
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
    min_value_dist <- min
    max_value_dist <- max

    # Identify outliers with IQR criterion
  } else if (method == 2) {
    data_dist <- data %>%
      dplyr::select(pop_id, pop_distance_source) %>%
      unique()
    min_value_dist <- quantile(
      data_dist$pop_distance_source, probs = 0.25, na.rm = TRUE) -
      1.5*IQR(data_dist$pop_distance_source, na.rm = TRUE)
    max_value_dist <- quantile(
      data_dist$pop_distance_source, probs = 0.75, na.rm = TRUE) +
      1.5*IQR(data_dist$pop_distance_source, na.rm = TRUE)

    if(is.null(percent) == FALSE) {
      min_value_dist <- min_value_dist - min_value_dist * percent / 100
      max_value_dist <- max_value_dist + max_value_dist * percent / 100
    }
    # Identify outliers with percentile method with interval of confidence
  } else if (method == 3) {
    num_lower <- ((100 - confidence.interval) / 2) / 100
    num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
    min_value_dist <- stats::quantile(data$pop_distance_source,
                                      num_lower, na.rm = TRUE)
    max_value_dist <- stats::quantile(data$pop_distance_source,
                                      num_upper, na.rm = TRUE)
  } else {
    stop("method inconnue")
  }

  commentaire <- paste0("Alertes en dehors de l'intervale [",
                        min_value_dist, "; ", max_value_dist, "]")
  print(commentaire)

  # Compute identification of alertes according to the chosen method
  data_dist <- data %>%
    dplyr::select(pop_id, pop_distance_source) %>%
    unique() %>%
    dplyr::mutate(
      dist_message = dplyr::case_when(
        is.na(pop_distance_source) == TRUE ~ "Distance source manquante",
        min_value_dist <= pop_distance_source &
          pop_distance_source <= max_value_dist ~
          "Distance source normale",
        min_value_dist > pop_distance_source |
          pop_distance_source > max_value_dist ~
          "Distance source anormale"
      ),
      dist_type = dplyr::case_when(
        is.na(pop_distance_source) == TRUE ~ "alerte",
        min_value_dist <= pop_distance_source &
          pop_distance_source <= max_value_dist ~ "",
        min_value_dist > pop_distance_source |
          pop_distance_source > max_value_dist ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the boxplot
  if (plot == TRUE) {
    g <- data_dist %>%
      dplyr::group_by(pop_id) %>%
      ggplot2::ggplot(aes(x = factor(0), y = pop_distance_source)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = dist_type)) +
      ggplot2::scale_color_manual(values = pal_alerte)+
      ggplot2::labs(x = "", y = "Distance source", color = "Types")
  }

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_dist) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(dist_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = pop_distance_source), size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des points de prelevements classes alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Distance source")
  }

  # Create statistics
  if (stat == TRUE) {
    dist_stat <- data_dist %>% # Percent
      plyr::summarise(
        na = round(
          mean(dist_message == "Distance source manquante") * 100,
          digits = 2),
        alerte = round(
          mean(dist_type == "alerte") * 100, digits = 2),
        dist.anormale = round(
          mean(dist_message == "Distance source anormale")* 100,
          digits = 2),
        dist.normale = round(
          mean(dist_message == "Distance source normale") * 100,
          digits = 2))

    dist_nb <- data_dist %>% # Number
      plyr::summarise(
        na = sum(
          dist_message == "Distance source manquante"),
        alerte = sum(
          dist_type == "alerte"),
        dist.anormale = sum(
          dist_message == "Distance source anormale"),
        dist.normale = sum(
          dist_message == "Distance source normale"))

    # create tables
    table_stat <- make_tab(x = dist_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Distance source (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = dist_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Distance source (nb)",
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
    utils::write.csv(data_dist, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_dist)
}

# ===========================

#' Verification relation entre distance_source, surface bassin versant et altitude
#' @description Fonction qui verifie si la surface du bassin versant augmente et l'altitude diminue avec l'augmentation de la distance a la source.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "pop_DS"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @export
#'
#' @seealso Pour verifier la coherence des valeurs de distance a la source : \code{\link{test_pop_dist}}. Pour verifier la coherence des valeurs de surface de bassin versant : \code{\link{test_pop_surfBV}}. Pour verifier la coherence des valeurs d'altitude : \code{\link{test_pop_alt}}.
#'
#' @examples
#' result_DS <- test_pop_DS(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE
#' )
#' @importFrom dplyr select group_by group_split mutate case_when inner_join filter
#' @importFrom ggplot2 ggplot geom_sf geom_point ggtitle labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom purrr map_df
#' @importFrom utils write.csv
test_pop_DS <- function(data,
                        stat = TRUE,
                        dir = getwd(),
                        file.name = "pop_DS",
                        map = TRUE,
                        csv = TRUE,
                        map.contour = IDF_contour,
                        map.cours.eau = cours_eau,
                        pdf = TRUE) {

  # Compute identification of alertes according to the protocole
  data_pop_DS <- data %>%
    dplyr::select(enh_libelle_sandre,
                  pop_surface_bassin_versant_amont,
                  pop_altitude,
                  pop_distance_source) %>%
    dplyr::filter(is.na(enh_libelle_sandre) == FALSE) %>%
    unique() %>%
    dplyr::group_by(enh_libelle_sandre) %>%
    dplyr::group_split() %>%
    purrr::map_df(
      .f = function(df1) {

        df1 <- df1[order(df1$pop_distance_source),]
        df1 <- df1 %>%
          dplyr::mutate(
            NA_message = dplyr::case_when(
              is.na(pop_surface_bassin_versant_amont) == TRUE |
                is.na(pop_altitude) == TRUE |
                is.na(pop_distance_source) == TRUE ~ "Valeur manquante",
              is.na(pop_surface_bassin_versant_amont) == FALSE &
                is.na(pop_altitude) == FALSE &
                is.na(pop_distance_source) == FALSE ~ "Aucune Valeur manquante"
            ),
            BVDS_message = dplyr::case_when(
              is.na(pop_surface_bassin_versant_amont) == TRUE ~
                "Surface BV manquante",
              is.unsorted(
                pop_surface_bassin_versant_amont, na.rm = TRUE) == FALSE ~
                "Surface BV conforme",
              is.unsorted(
                pop_surface_bassin_versant_amont, na.rm = TRUE) == TRUE ~
                "Surface BV non conforme"
            ),
            altDS_message= dplyr::case_when(
              is.na(pop_altitude) == TRUE ~ "Altitude manquante",
              is.unsorted(rev(pop_altitude), na.rm = TRUE) == FALSE
              ~ "Altitude conforme",
              is.unsorted(rev(pop_altitude), na.rm = TRUE) == TRUE
              ~ "Altitude non conforme"
            ),
            DS_type = dplyr::case_when(
              is.na(pop_surface_bassin_versant_amont) == TRUE |
                is.na(pop_altitude) == TRUE |
                is.na(pop_distance_source) == TRUE ~ "alerte",
              # is.na(pop_surface_bassin_versant_amont) == TRUE ~
              #   "alerte",
              # is.na(pop_altitude) == TRUE ~ "alerte",
              is.unsorted(
                pop_surface_bassin_versant_amont, na.rm = TRUE) == FALSE ~ "",
              is.unsorted(
                pop_surface_bassin_versant_amont, na.rm = TRUE) == TRUE ~
                "alerte",
              is.unsorted(rev(pop_altitude), na.rm = TRUE) == FALSE
              ~ "",
              is.unsorted(rev(pop_altitude), na.rm = TRUE) == TRUE
              ~ "alerte",
              is.na(pop_surface_bassin_versant_amont) == FALSE &
                is.na(pop_altitude) == FALSE &
                is.na(pop_distance_source) == FALSE ~ ""
            )
          )
      }
    )

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_pop_DS) %>%
      dplyr::group_by(pop_id) %>%
      dplyr::filter(DS_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = IDF_contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = cours_eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y),
                          size = 2) +
      ggplot2::ggtitle("Carte des points de prelevements classees alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude")
  }

  # Create statistics
  if (stat == TRUE) {
    PopDS_stat <- data_pop_DS %>%
      plyr::summarise(
        na = round(
          mean(NA_message == "Valeur manquante") * 100,
          digits = 2),
        alerte = round(
          mean(DS_type == "alerte") * 100, digits = 2),
        alt.normale = round(
          mean(altDS_message == "Altitude conforme") * 100, digits = 2),
        alt.anormale = round(
          mean(altDS_message == "Altitude non conforme") * 100,
          digits = 2),
        BV.normale = round(
          mean(BVDS_message == "Surface BV conforme") * 100, digits = 2),
        BV.anormale = round(
          mean(BVDS_message == "Surface BV non conforme") * 100,
          digits = 2))

    PopDS_nb <- data_pop_DS %>%
      plyr::summarise(
        na.alt = sum(NA_message == "Valeur manquante"),
        alerte = sum(DS_type == "alerte"),
        alt.normale = sum(altDS_message == "Altitude conforme"),
        alt.anormale = sum(altDS_message == "Altitude non conforme"),
        BV.normale = sum(BVDS_message == "Surface BV conforme"),
        BV.anormale = sum(BVDS_message == "Surface BV non conforme"))

    # Create tables
    commentaire <- c("Alertes non conformite altitude et surface BV en fonction distance source")

    table_stat <- make_tab(x = PopDS_stat,
                           comment.size = 7,
                           colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                       "#BF3111", "#5AB5BF", "#BF3111"),
                           titre = "Rapport DS-altitude-SBV (%)",
                           comment = commentaire,
                           size.tab = 7)
    table_nb <- make_tab(x = PopDS_nb,
                         comment.size = 7,
                         colours = c("#BF3111", "#BF3111", "#5AB5BF",
                                     "#BF3111", "#5AB5BF", "#BF3111"),
                         titre = "Rapport DS-altitude-SBV (nb)",
                         comment = commentaire,
                         size.tab = 7)
  }

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
    utils::write.csv(data_pop_DS, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_pop_DS)
}
