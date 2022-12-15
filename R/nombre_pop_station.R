# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Nombre de point de prelevement par station
#' @description Une fonction permettant de determiner le nombre de point de prelevement par station et donnant des alertes dans le cas ou il y en aurait plusieurs.
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "sta_nbPop"`.
#' @param map Argument logique `TRUE/FALSE`. Par defaut `map = TRUE` et une carte des alertes est tracee.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @param color.point Vecteur. Contient les couleurs des points sur la carte entre `""`. Par defaut, `color.point = divergent_24`.
#' @param map.contour Format sf. Donnees de contour de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.contour = IDF_contour` qui correspond au fond de carte d'Ile de France.
#' @param map.cours.eau Format sf. Donnees des cours d'eau de la zone etudiee a charger avec la fonction \code{\link[sf]{st_read}}. Par defaut, `map.cours.eau = cours_eau` qui correspond aux cours d'eau Ile de France.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @return Des resultats graphiques : carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @export
#'
#' @examples
#' result_nombrePop <- test_sta_nombrePop(
#' data = exemple_IDF,
#' csv = FALSE,
#' pdf = FALSE)
#'
#' @importFrom dplyr filter group_by summarise n_distinct mutate case_when inner_join
#' @importFrom ggplot2 ggplot geom_sf geom_point scale_color_manual ggtitle labs
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_sta_nombrePop <- function(data,
                               stat = TRUE,
                               dir = getwd(),
                               file.name = "sta_nbPop",
                               map = TRUE,
                               csv = TRUE,
                               color.point = divergent_24,
                               map.contour = IDF_contour,
                               map.cours.eau = cours_eau,
                               pdf = TRUE) {

  dataNoStation <- data %>%
    dplyr::filter(!is.na(pop_sta_id) == TRUE)

  if (nrow(data) != nrow(dataNoStation))
    message(
      paste0(
        nrow(data) - nrow(dataNoStation),
        " enregistrements sans code station associe"
      )
    )

  DoublePop <- dataNoStation %>%
    dplyr::group_by(sta_code_sandre) %>%
    dplyr::summarise(nb_op = dplyr::n_distinct(pop_id)) %>%
    dplyr::mutate(
      nombreOp_message = dplyr::case_when(
        nb_op == 0 ~ "Pas de point de prelevement associe",
        nb_op == 1 ~ "Un point de prelevement associe",
        nb_op > 1 ~ "Plusieurs points de prelevement associes"
      ),
      nombreOp_type = dplyr::case_when(
        nb_op == 0 ~ "alerte",
        nb_op == 1 ~ "",
        nb_op > 1 ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create the map
  if (map == TRUE) {
    g1 <- data %>%
      dplyr::inner_join(DoublePop) %>%
      dplyr::filter(nombreOp_type == "alerte") %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = map.contour, colour = "grey60", fill = "grey85") +
      ggplot2::geom_sf(data = map.cours.eau, colour = "grey55") +
      ggplot2::geom_point(aes(x = pop_coordonnees_x, y = pop_coordonnees_y,
                              color = as.factor(sta_code_sandre)), size = 2) +
      ggplot2::scale_color_manual(values = color.point)+
      ggplot2::ggtitle("Carte des stations avec deux points de prelevement") +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    color = "Double points de prelevement")
  }

  # Create statistics
  if (stat == TRUE) {
    DoublePop_stat <- DoublePop %>%
      plyr::summarise(
        na = round(
          mean(nombreOp_message == "Pas de point de prelevement associe") * 100,
          digits = 2),
        alerte = round(
          mean(nombreOp_type == "alerte") * 100, digits = 2),
        one.pop = round(
          mean(nombreOp_message == "Un point de prelevement associe") * 100,
          digits = 2),
        double.pop = round(
          mean(nombreOp_message == "Plusieurs points de prelevement associes")
          * 100, digits = 2))

    DoublePop_nb <- DoublePop %>% # Number
      plyr::summarise(
        na = sum(
          nombreOp_message == "Pas de point de prelevement associe"),
        alerte = sum(
          nombreOp_type == "alerte"),
        one.pop = sum(
          nombreOp_message == "Un point de prelevement associe"),
        double.pop = sum(
          nombreOp_message == "Plusieurs points de prelevement associes"))

    # Create tables
    commentaire <-
      c("Alertes stations avec plusieurs points de prelevement")

    table_stat <- make_tab(x = DoublePop_stat,
                           colours = c("#BF3111", "#BF3111", "#BF3111",
                                       "#5AB5BF", "#BF3111", "#5AB5BF"),
                           titre = "Rapport nb de points de prelevement (%)",
                           comment = commentaire,
                           size.tab = 12,
                           size.titre = 10)
    table_nb <- make_tab(x = DoublePop_nb,
                         colours = c("#BF3111", "#BF3111", "#BF3111",
                                     "#5AB5BF", "#BF3111", "#5AB5BF"),
                         titre = "Rapport nb de points de prelevement (nb)",
                         comment = commentaire,
                         size.tab = 12,
                         size.titre = 10)
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
    utils::write.csv(DoublePop, paste0(dir, "/", file.name, ".csv"),
                     row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(DoublePop)
}

# ===========================
