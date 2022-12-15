# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Coherence des valeurs de profondeur, largeur et longueur.
#' @description Test qui (1) calcule une longueur theorique minimum de l'operation en fonction de la largeur, (2) determine si le protocole de peche est adapte en fonction de la largeur et de la longueur de l'operation, et (3) compare la valeur de longueur theorique à celle realisee.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot des ecarts de longeur est trace.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "ope_mesure"`.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param color_alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color_alerte = pal_alerte`.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param percent Numerique. Pourcentage d'ecart a la valeur theorique de longueur calculee.
#' @return Des resultats graphiques : boxplot (`plot = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @seealso Attention ! Pour utiliser cette fonction, il est preferable de verifier les valeurs de profondeur (\code{\link{test_ope_profondeur}}), de largeur (\code{\link{test_ope_Lar}})et de longueur (\code{\link{test_ope_Lon}}).
#'
#' @examples
#' result_mesure <- test_ope_mesure(
#' data = exemple_IDF,
#' csv = FALSE,
#' pdf = FALSE)
#'
#' @importFrom dplyr select mutate case_when group_by filter
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_color_manual labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_ope_mesure <- function(data,
                            stat = TRUE,
                            dir = getwd(),
                            plot = TRUE,
                            file.name = "ope_mesure",
                            csv = TRUE,
                            color_alerte = pal_alerte,
                            pdf = TRUE,
                            percent = 0) {

  data_mesure <- data %>%
    dplyr::select(ope_id, ope_pro_id, odp_largeur_lame_eau, odp_longueur,
                  ode_profondeur_moyenne_station) %>%
    unique() %>%
    # Calcul de la longeur theorique en fonction de la largeur mesuree
    dplyr::mutate(
      theoLong = dplyr::case_when(
        is.na(odp_largeur_lame_eau) == TRUE ~ 0,
        odp_largeur_lame_eau < 3 ~ 60,
        (odp_largeur_lame_eau >= 3 & odp_largeur_lame_eau <= 30) ~
          (20 * odp_largeur_lame_eau),
        (odp_largeur_lame_eau > 30 & odp_largeur_lame_eau <= 60 ) ~ 600,
        odp_largeur_lame_eau > 60 ~ (10 * odp_largeur_lame_eau)
      ),
      # Identifier si protocole correspond a bonne profondeur et largeur
      theoPro_message = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "Pas de protocole renseigne",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station < 0.7 &
                             odp_largeur_lame_eau < 9) ~
          "Protocole peche complete adapte",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station > 0.7 |
                             odp_largeur_lame_eau > 9) ~
          "Protocole peche complete non adapte",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station < 0.7 &
                             odp_largeur_lame_eau < 9) ~
          "Protocole peche partielle non adapte",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station > 0.7 |
                             odp_largeur_lame_eau > 9) ~
          "Protocole peche partielle adapte"
      ),
      theoPro_type = dplyr::case_when(
        is.na(ope_pro_id) == TRUE ~ "alerte",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station < 0.7 &
                             odp_largeur_lame_eau < 9) ~ "",
        ope_pro_id == 1 & (ode_profondeur_moyenne_station > 0.7 |
                             odp_largeur_lame_eau > 9) ~ "alerte",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station < 0.7 &
                             odp_largeur_lame_eau < 9) ~ "alerte",
        ope_pro_id == 2 & (ode_profondeur_moyenne_station > 0.7 |
                             odp_largeur_lame_eau > 9) ~ "",
        TRUE ~ ""
      ),
      theoLong_message = dplyr::case_when(
        is.na(theoLong) == TRUE ~ "Absence longueur",
        theoLong <= (odp_longueur + odp_longueur * percent / 100) ~
          "Longueur associee normale",
        theoLong > (odp_longueur + odp_longueur * percent / 100) ~
          "Longueur associee anormale",
        TRUE ~ ""
      ),
      theoLong_type = dplyr::case_when(
        is.na(theoLong) == TRUE ~ "alerte",
        theoLong <= (odp_longueur + odp_longueur * percent / 100) ~
          "",
        theoLong > (odp_longueur + odp_longueur * percent / 100) ~
          "alerte",
        TRUE ~ ""
      ),
      # Calcul ecart entre longueur theorique et calculee
      theoLong_ecart = dplyr::case_when(
        is.na(theoLong) == TRUE ~ 0,
        theoLong <= odp_longueur ~ 0,
        theoLong > (odp_longueur + odp_longueur * percent / 100) ~
          abs(theoLong - (odp_longueur + odp_longueur * percent / 100)),
        TRUE ~ 0
      )
    )

  # Create boxplot of ecart
  if (plot == TRUE & any(data_mesure$theoLong_ecart > 0) == TRUE) {
    g <- data_mesure %>%
      dplyr::group_by(ope_id) %>%
      dplyr::filter(theoLong_ecart != 0) %>%
      ggplot2::ggplot(aes(x = factor(0), y = theoLong_ecart)) +
      ggplot2::geom_boxplot(fill = "grey60") +
      ggplot2::geom_point(aes(color = as.factor(ope_pro_id))) +
      ggplot2::scale_color_manual(values = pal_alerte) +
      ggplot2::labs(x = "", y = "Valeurs des ecarts de longueur",
                    color = "Protocole")
  }

  # Create statistics
  if (stat == TRUE) {
    mesure_stat <- data_mesure %>%
      plyr::summarise(
        peche.part.normale = round(
          mean(theoPro_message == "Protocole peche partielle adapte") * 100,
          digits = 2),
        peche.part.anormale = round(
          mean(theoPro_message == "Protocole peche partielle non adapte") * 100,
          digits = 2),
        peche.compl.normale = round(
          mean(theoPro_message == "Protocole peche complete adapte") * 100,
          digits = 2),
        peche.compl.anormale = round(
          mean(theoPro_message == "Protocole peche complete non adapte") * 100,
          digits = 2),
        pro.na = round(mean(theoPro_message == "Pas de protocole renseigne")
                       * 100, digits = 2),
        alerte.pro = round(
          mean(theoPro_type == "alerte") * 100, digits = 2),
        Long.normale = round(
          mean(theoLong_message == "Longueur associee normale") * 100,
          digits = 2),
        Long.anormale = round(
          mean(theoLong_message == "Longueur associee anormale") * 100,
          digits = 2),
        Long.na = round(
          mean(theoLong_message == "Absence longueur") * 100, digits = 2),
        alerte.Long = round(
          mean(theoLong_type == "alerte") * 100, digits = 2))

    mesure_nb <- data_mesure %>%
      plyr::summarise(
        peche.part.normale = sum(
          theoPro_message == "Protocole peche partielle adapte"),
        peche.part.anormale = sum(
          theoPro_message == "Protocole peche partielle non adapte"),
        peche.compl.normale = sum(
          theoPro_message == "Protocole peche complete adapte"),
        peche.compl.anormale = sum(
          theoPro_message == "Protocole peche complete non adapte"),
        pro.na = sum(
          theoPro_message == "Pas de protocole renseigne"),
        alerte.pro = sum(theoPro_type == "alerte"),
        Long.normale = sum(
          theoLong_message == "Longueur associee normale"),
        Long.anormale = sum(
          theoLong_message == "Longueur associee anormale"),
        Long.na = sum(theoLong_message == "Absence longueur"),
        alerte.Long = sum(theoLong_type == "alerte"))

    # Create tables
    commentaire <- c(
      "Alertes protocole en rapport avec Longeur, Largeur et Profondeur")

    table_stat <- make_tab(x = mesure_stat,
                           colours = c("#5AB5BF", "#BF3111", "#5AB5BF",
                                       "#BF3111", "#BF3111", "#BF3111",
                                       "#5AB5BF", "#BF3111", "#BF3111",
                                       "#BF3111"),
                           titre = "Rapport protocole et mesures associees (%)",
                           comment = commentaire, size.tab = 5)
    table_nb <- make_tab(x = mesure_nb,
                         colours = c("#5AB5BF", "#BF3111", "#5AB5BF",
                                     "#BF3111", "#BF3111", "#BF3111",
                                     "#5AB5BF", "#BF3111", "#BF3111",
                                     "#BF3111"),
                         titre = "Rapport protocole et mesures associees (nb)",
                         comment = commentaire, size.tab = 5)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (plot == TRUE) {
      print(g) # boxplot
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
  # Write CSV
  if (csv == TRUE) {
    utils::write.csv(data_mesure, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_mesure)
}
