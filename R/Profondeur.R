# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de profondeur
#' @description Quatre methodes disponibles afin de tester la coherence des valeurs de profondeur.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param min Valeur minimum a choisir si `method = 1`.
#' @param max Valeur maximum a choisir si `method = 1`.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_profondeur"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param method Trois methodes pour identifier les outliers. Si `method = 1`, les valeurs en dehors de l'interval defini par le `min` et le `max` sont considerees comme des outliers. Si `method = 2`, les valeurs en dehors de l'interval defini par l'IQR criterion sont considerees comme des outliers. Si `method = 3`, les valeurs en dehors de l'interval defini par un interval de confiance (`confidence.interval`) sont considerees comme des outliers. Si `method = 4`, l'IQR est cette fois applique en groupant les operations par point de prelevement.
#' @param confidence.interval Numerique. Interval de confiance pris en compte si `method = 3`. Par defaut `confidence.interval = 95`.
#' @param color_alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Pourcentage d'ecart aux valeurs seuils si `method = 2`.
#' @param echelle Deux valeurs possibles. Si `echelle = "type_cours_eau"` alors les valeurs aberrantes sont estimees a l'echelle des types de cours d'eau (voir \code{\link{getDataRegion}} pour inclure type de cours d'eau). Si `echelle = "POP"` alors les valeurs aberrantes sont estimees a l'echelle des points de prelevement.

#' @details En ce qui concerne l'IQR criterion (`method = 2` et `method = 4`), les valeurs considerees comme aberrantes sont celles qui sont superieures a `quantile(x, 3/4) + IQR(x)` et celles qui sont inferieures a `quantile(x, 1/4) - IQR(x)`. Pour plus de details sur le calcul voir les fonctions suivantes : \code{\link[stats]{IQR}} et \code{\link[stats]{quantile}}.
#'
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Une fonction permettant de verifier le calcul des valeurs de profondeur : \code{\link{test_ope_ProfFacies}}. Une fonction exploitant la relation entre la largeur, la profondeur et la longueur de l'operation : \code{\link{test_ope_mesure}}.
#'
#' @examples
#' result_profondeur <- test_ope_profondeur(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' method = 4)
#'
#' @importFrom dplyr select mutate case_when group_by group_split inner_join filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point labs scale_color_manual coord_flip ggtitle geom_sf
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom purrr map_df
#' @importFrom stats quantile
#' @importFrom utils write.csv
#' @importFrom viridis scale_color_viridis

test_ope_profondeur <- function(data,
                                plot = TRUE,
                                stat = TRUE,
                                min = 100,
                                max = 2000,
                                dir = getwd(),
                                file.name = "ope_profondeur",
                                map = TRUE,
                                csv = TRUE,
                                method = 3,
                                confidence.interval = 95,
                                color.alerte = pal_alerte,
                                map.contour = IDF_contour,
                                map.cours.eau = cours_eau,
                                pdf = TRUE,
                                percent = NULL,
                                echelle = "POP") {

  if (method == 1 | method == 2 | method == 3) {
    # Identify outliers according to an interval
    if (method == 1) {
      min_value_profondeur <- min
      max_value_profondeur <- max

      # Identify outliers with IQR criterion
    } else if (method == 2) {
      data_profondeur <- data %>%
        dplyr::select(ope_id, ode_profondeur_moyenne_station,
                      enh_libelle_sandre) %>%
        unique()

      min_value_profondeur <- quantile(
        data_profondeur$ode_profondeur_moyenne_station,
        probs = 0.25, na.rm = TRUE) -
        1.5*IQR(data_profondeur$ode_profondeur_moyenne_station, na.rm = TRUE)
      max_value_profondeur <- quantile(
        data_profondeur$ode_profondeur_moyenne_station,
        probs = 0.75, na.rm = TRUE) +
        1.5*IQR(data_profondeur$ode_profondeur_moyenne_station, na.rm = TRUE)

      if(is.null(percent) == FALSE) {
        min_value_profondeur <- min_value_profondeur -
          min_value_profondeur * percent / 100
        max_value_profondeur <- max_value_profondeur +
          max_value_profondeur * percent / 100
      }

      # Identify outliers with percentile method with interval of confidence
    } else if (method == 3) {
      num_lower <- ((100 - confidence.interval) / 2) / 100
      num_upper <- (100 - ((100 - confidence.interval) / 2)) / 100
      min_value_profondeur <- stats::quantile(
        data$ode_profondeur_moyenne_station, num_lower, na.rm = TRUE)
      max_value_profondeur <- stats::quantile(
        data$ode_profondeur_moyenne_station, num_upper, na.rm = TRUE)
    }
    commentaire <- paste0("Alertes en dehors de l'intervale [",
                          min_value_profondeur, "; ",
                          max_value_profondeur, "]")
    print(commentaire)

    # Compute identification of alertes according to the method chosen
    data_profondeur <- data %>%
      dplyr::select(ope_id,
                    ode_profondeur_moyenne_station,
                    enh_libelle_sandre) %>%
      unique() %>%
      dplyr::mutate(
        Prof_message = dplyr::case_when(
          is.na(ode_profondeur_moyenne_station) == TRUE ~
            "Profondeur manquante",
          min_value_profondeur <= ode_profondeur_moyenne_station &
            ode_profondeur_moyenne_station <= max_value_profondeur ~
            "Profondeur normale",
          min_value_profondeur > ode_profondeur_moyenne_station |
            ode_profondeur_moyenne_station > max_value_profondeur ~
            "Profondeur anormale"
        ),
        Prof_type = dplyr::case_when(
          is.na(ode_profondeur_moyenne_station) == TRUE ~ "alerte",
          min_value_profondeur <= ode_profondeur_moyenne_station &
            ode_profondeur_moyenne_station <= max_value_profondeur ~ "",
          min_value_profondeur > ode_profondeur_moyenne_station |
            ode_profondeur_moyenne_station > max_value_profondeur ~ "alerte",
          TRUE ~ ""
        )
      )

    # Create the boxplot
    if (plot == TRUE) {
      g <- data_profondeur %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = factor(0), y = ode_profondeur_moyenne_station)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Prof_type)) +
        ggplot2::labs(x = "", y = "Profondeur", color = "alerte") +
        ggplot2::scale_color_manual(values = pal_alerte)

      g3 <- data_profondeur %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = as.factor(enh_libelle_sandre),
                            y = ode_profondeur_moyenne_station)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Prof_type)) +
        coord_flip() +
        ggplot2::labs(x = "Nom cours eau", y = "Profondeur", color = "alerte") +
        ggplot2::scale_color_manual(values = pal_alerte)
    }

  } else if (method == 4) {

    if (echelle == "POP") {
      var <- dplyr::quo(pop_id)
      commentaire <- "alerte IQR criterion par points de prelevement"
      typ_eau <- 0
      legende_x <- "Point de prélèvement"
    } else if (echelle == "type_cours_eau") {
      var <- dplyr::quo(Type_cours_eau)
      commentaire <- "alerte IQR criterion par type cours d'eau"
      typ_eau <- c("Type_cours_eau")
      legende_x <- "Type de cours d'eau"
    } else if (echelle != "POP" | echelle != "type_cours_eau") {
      stop("echelle inconnue : choisir entre POP ou type_cours_eau")
    }

    data_profondeur <- data %>%
      dplyr::select(ope_id, ode_profondeur_moyenne_station, pop_id,
                    enh_libelle_sandre, typ_eau) %>%
      unique() %>%
      dplyr::group_by(!!var) %>%
      dplyr::group_split() %>%
      purrr::map_df(
        .f = function(df1) {

          min_value_profondeur = quantile(
            df1$ode_profondeur_moyenne_station, probs = 0.25, na.rm = TRUE) -
            1.5*IQR(df1$ode_profondeur_moyenne_station, na.rm = TRUE)

          max_value_profondeur = quantile(
            df1$ode_profondeur_moyenne_station, probs = 0.75, na.rm = TRUE) +
            1.5*IQR(df1$ode_profondeur_moyenne_station, na.rm = TRUE)

          if(is.null(percent) == FALSE) {
            min_value_profondeur <- min_value_profondeur -
              min_value_profondeur * percent / 100
            min_value_profondeur <- min_value_profondeur +
              min_value_profondeur * percent / 100
          }

          df1 <- df1 %>%
            dplyr::mutate(
              Prof_message = dplyr::case_when(
                is.na(ode_profondeur_moyenne_station) == TRUE ~
                  "Profondeur manquante",
                min_value_profondeur <= ode_profondeur_moyenne_station &
                  ode_profondeur_moyenne_station <= max_value_profondeur ~
                  "Profondeur normale",
                min_value_profondeur > ode_profondeur_moyenne_station |
                  ode_profondeur_moyenne_station > max_value_profondeur ~
                  "Profondeur anormale"
              ),
              Prof_type = dplyr::case_when(
                is.na(ode_profondeur_moyenne_station) == TRUE ~ "alerte",
                min_value_profondeur <= ode_profondeur_moyenne_station &
                  ode_profondeur_moyenne_station <= max_value_profondeur ~ "",
                min_value_profondeur > ode_profondeur_moyenne_station |
                  ode_profondeur_moyenne_station > max_value_profondeur ~
                  "alerte",
                TRUE ~ ""
              ),
              ecart_prof = dplyr::case_when(
                Prof_message == "Profondeur manquante" ~ NA_real_,
                Prof_message == "Profondeur normale" ~ 0,
                Prof_message == "Profondeur anormale" &
                  ode_profondeur_moyenne_station > max_value_profondeur ~
                  ode_profondeur_moyenne_station - max_value_profondeur,
                Prof_message == "Profondeur anormale" &
                  ode_profondeur_moyenne_station < min_value_profondeur ~
                  min_value_profondeur - ode_profondeur_moyenne_station
              )
            )
        }
      )
    if (plot == TRUE) {
      g4 <- data_profondeur %>%
        dplyr::group_by(ope_id) %>%
        ggplot2::ggplot(aes(x = as.factor(!!var),
                            y = ode_profondeur_moyenne_station)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point(aes(color = Prof_type)) +
        ggplot2::labs(x = legende_x, y = "Profondeur") +
        ggplot2::scale_color_manual(values = pal_alerte) +
        ggplot2::coord_flip()

      g5 <- data_profondeur %>%
        ggplot2::ggplot(aes(x = factor(0),
                            y = ecart_prof)) +
        ggplot2::geom_boxplot(fill = "grey60") +
        ggplot2::geom_point() +
        ggplot2::labs(x = "", y = "Valeurs des ecarts de profondeur") +
        ggplot2::ggtitle("Boxplot des ecarts au max/min des alertes")
    }



  } else {
    stop("method inconnue")
  }

  # Create the map
  if(map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(data_profondeur) %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(Prof_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = ode_profondeur_moyenne_station),
                          size = 2) +
      viridis::scale_color_viridis(option = "magma", direction = -1) +
      ggplot2::ggtitle("Carte des valeurs de profondeur en alerte") +
      ggplot2::labs(x = "Longitude", y = "Latitude", color = "Profondeur")
  }

  # Create statistics
  if (stat == TRUE) {
    Prof_stat <- data_profondeur %>%
      plyr::summarise(
        na = round(
          mean(Prof_message == "Profondeur manquante") * 100,
          digits = 2),
        alerte = round(
          mean(Prof_type == "alerte") * 100, digits = 2),
        Prof.anormale = round(
          mean(Prof_message == "Profondeur anormale") * 100,
          digits = 2),
        Prof.normale = round(
          mean(Prof_message == "Profondeur normale") * 100,
          digits = 2))

    Prof_nb <- data_profondeur %>% #number
      plyr::summarise(
        na = sum(Prof_message == "Profondeur manquante"),
        alerte = sum(Prof_type == "alerte"),
        Prof.anormale = sum(Prof_message == "Profondeur anormale"),
        Prof.normale = sum(Prof_message == "Profondeur normale"))

    # create tables
    table_stat <- make_tab(x = Prof_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport Profondeur (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = Prof_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport Profondeur (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      if (method == 1 | method == 2 | method == 3) {
        print(g) # boxplot
        print(g3) # boxplot for cours eau
      } else if (method == 4) {
        print(g4)
        print(g5)
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
      print(g) # boxplot
      grid::grid.newpage()
      print(g3) # boxplot for cours eau
    } else if (method == 4) {
      print(g4)
      grid::grid.newpage()
      print(g5)
    } else {
      print("no plot draw ==> write plot = TRUE to see it")
    }
  }
  # Plot the map
  if (map == TRUE) {
    print(g1)
  } else {
    print("no map draw ==> write map = TRUE to see it")
  }
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_profondeur, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_profondeur)
}


# ===========================

#' Verification du calcul de la profondeur
#' @description Verifie que la profondeur des peches partielles correspond bien à la somme des profondeurs des facies.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_ProfFacies"`.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @return Des resultats sous forme de tableaux (`stat = TRUE`). Un pdf reprenant ces tableaux. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_ProfFacies <- test_ope_ProfFacies(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE)
#'
#' @seealso Une fonction permettant de verifier la coherence des valeurs de profondeur : \code{\link{test_ope_profondeur}}. Une fonction exploitant la relation entre la largeur, la profondeur et la longueur de l'operation : \code{\link{test_ope_mesure}}.
#'
#'
#' @importFrom dplyr select group_by mutate case_when
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_ope_ProfFacies <- function(data,
                                stat = TRUE,
                                dir = getwd(),
                                file.name = "ope_ProfFacies",
                                csv = TRUE,
                                pdf = TRUE) {
  data_fac <- data %>%
    dplyr::select(ope_id,
                  fac_profondeur_moyenne,
                  fac_importance_relative,
                  fac_tyf_id)

  data_ProfFacies <- data %>%
    dplyr::select(ope_id,
                  ope_pro_id,
                  fac_tyf_id,
                  fac_profondeur_moyenne,
                  fac_importance_relative,
                  ode_profondeur_moyenne_station) %>%
    unique() %>%
    dplyr::group_by(ope_id) %>%
    # Calcul de la profondeur theorique en fonction des prof des facies
    dplyr::mutate(
      theoProf = dplyr::case_when(
        is.na(ode_profondeur_moyenne_station) == TRUE ~ NA_real_,
        ope_pro_id == 1 | ope_pro_id == 2 ~
          round(sum((fac_profondeur_moyenne * fac_importance_relative)/100,
                    na.rm = TRUE), digits = 2)
        # ope_pro_id == 2 & (is.na(fac_profondeur_moyenne) == TRUE |
        #                      is.na(fac_importance_relative) == TRUE) ~ 0,
      ),
      ProfFacies_message = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "Pas de protocole renseigne",
        is.na(theoProf) == TRUE | is.null(theoProf) == TRUE ~
          "Valeur de profondeur manquante",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station == theoProf) ~
          "Bon calcul de profondeur pour peche complete",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station != theoProf) ~
          "Mauvais calcul de profondeur pour peche complete",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station == theoProf) ~
          "Bon calcul de profondeur pour peche partielle",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station != theoProf) ~
          "Mauvais calcul de profondeur pour peche partielle"
      ),
      ProfFacies_type = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "alerte",
        is.na(theoProf) == TRUE | is.null(theoProf) == TRUE ~
          "alerte",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station == theoProf) ~
          "",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station != theoProf) ~
          "alerte",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station == theoProf) ~
          "",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station != theoProf) ~
          "alerte",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(-fac_profondeur_moyenne,
                  -fac_importance_relative,
                  -fac_tyf_id) %>%
    unique()

  # Create statistics
  if (stat == TRUE) {
    ProfFacies_stat <- data_ProfFacies %>%
      plyr::summarise(
        prof.PC.normale = round(
          mean(ProfFacies_message ==
                 "Bon calcul de profondeur pour peche complete") * 100,
          digits = 2),
        prof.PC.anormale = round(
          mean(ProfFacies_message ==
                 "Mauvais calcul de profondeur pour peche complete") * 100,
          digits = 2),
        prof.PP.normale = round(
          mean(ProfFacies_message ==
                 "Bon calcul de profondeur pour peche partielle") * 100,
          digits = 2),
        prof.PP.anormale = round(
          mean(ProfFacies_message ==
                 "Mauvais calcul de profondeur pour peche partielle") * 100,
          digits = 2),
        pro.na = round(
          mean(ProfFacies_message ==
                 "Pas de protocole renseigne") * 100, digits = 2),
        param.na = round(
          mean(ProfFacies_message ==
                 "Valeur de profondeur manquante") * 100, digits = 2),
        alerte = round(
          mean(ProfFacies_type == "alerte") * 100, digits = 2))

    ProfFacies_nb <- data_ProfFacies %>%
      plyr::summarise(
        prof.PC.normale = sum(ProfFacies_message ==
                                "Bon calcul de profondeur pour peche complete"),
        prof.PC.anormale = sum(ProfFacies_message ==
                                 "Mauvais calcul de profondeur pour peche complete"),
        prof.PP.normale = sum(ProfFacies_message ==
                                "Bon calcul de profondeur pour peche partielle"),
        prof.PP.anormale = sum(ProfFacies_message ==
                                 "Mauvais calcul de profondeur pour peche partielle"),
        pro.na = sum(ProfFacies_message ==
                       "Pas de protocole renseigne"),
        param.na = sum(ProfFacies_message == "Valeur de profondeur manquante"),
        alerte = sum(ProfFacies_type == "alerte"))

    # Create tables
    commentaire <- c(
      "Alertes profondeur theorique differente de calculee")

    table_stat <- make_tab(x = ProfFacies_stat,
                           colours = c("#5AB5BF", "#BF3111", "#5AB5BF",
                                       "#BF3111", "#BF3111", "#BF3111",
                                       "#BF3111"),
                           titre = "Rapport profondeur et facies (%)",
                           comment = commentaire, size.tab = 7)
    table_nb <- make_tab(x = ProfFacies_nb,
                         colours = c("#5AB5BF", "#BF3111", "#5AB5BF",
                                     "#BF3111", "#BF3111", "#BF3111",
                                     "#BF3111"),
                         titre = "Rapport profondeur et facies (nb)",
                         comment = commentaire, size.tab = 7)
  }

  data_ProfFacies <- data_ProfFacies %>%
    left_join(data_fac) %>%
    unique()

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (stat == TRUE) {
      grid::grid.newpage()
      grid::grid.draw(table_stat) # percent
      grid::grid.newpage()
      grid::grid.draw(table_nb) # nb
    }
    if (stat == FALSE) {
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

  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_ProfFacies, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_ProfFacies)
}

# ===========================
