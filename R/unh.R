# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Conformite des valeurs d'unite hydrographique
#' @description Verifie que l'unite hydrographique correspond bien a celle de la zone etudiee.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}.
#' @param stat Argument logique `TRUE/FALSE`. Par defaut `stat = TRUE` et deux tableaux contenants les resultats de la fonction sont affiches.
#' @param dir Chemin d'acces ou enregistrer les fichiers de sortie. Par defaut regle sur le repertoire de travail (`dir = `\code{\link[base]{getwd}}).
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "pop_UNH"`.
#' @param unite.hydro Code l'unite hydro en question. Par defaut regle sur la zone IDF (`unite.hydro = "H2"`).
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).
#' @return Des resultats graphiques : boxplot (`plot = TRUE`), carte (`map = TRUE`) et tableaux (`stat = TRUE`). Un pdf reprenant ces precedents resultats. Et un tableau au format csv contenant le detail des alertes.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @export
#'
#' @examples
#' test_UNH <- test_pop_UNH(
#' data = exemple_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' unite.hydro = "H2")

#' @importFrom dplyr group_by summarise mutate case_when
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.draw
#' @importFrom plyr summarise
#' @importFrom utils write.csv

test_pop_UNH <- function(data,
                         stat = TRUE,
                         dir = getwd(),
                         file.name = "pop_UNH",
                         unite.hydro = "H2",
                         csv = TRUE,
                         pdf = TRUE) {

  data_UNH <- data %>%
    dplyr::group_by(pop_id) %>%
    dplyr::summarise(UNH = unique(unh_code_sandre)) %>%
    dplyr::mutate(
      UNH_message = dplyr::case_when(
        is.na(UNH) == TRUE ~ "Pas d'unite hydrographique associee",
        UNH == unite.hydro ~ "Unite hydrographique correspondante",
        UNH != unite.hydro ~ "Mauvaise unite hydrographique"
      ),
      UNH_type = dplyr::case_when(
        is.na(UNH) == TRUE ~ "alerte",
        UNH == unite.hydro ~ "",
        UNH != unite.hydro ~ "alerte",
        TRUE ~ ""
      )
    )

  # Create statistics
  if (stat == TRUE) {
    UNH_stat <- data_UNH %>%
      plyr::summarise(
        na = round(
          mean(UNH_message == "Pas d'unite hydrographique associee") * 100,
          digits = 2),
        alerte = round(
          mean(UNH_type == "alerte") * 100, digits = 2),
        UNH.anormale = round(
          mean(UNH_message == "Mauvaise unite hydrographique") * 100,
          digits = 2),
        UNH.normale = round(
          mean(UNH_message == "Unite hydrographique correspondante") * 100,
          digits = 2))

    UNH_nb <- data_UNH %>% # Number
      plyr::summarise(
        na = sum(
          UNH_message == "Pas d'unite hydrographique associee"),
        alerte = sum(
          UNH_type == "alerte"),
        UNH.anormale = sum(
          UNH_message == "Mauvaise unite hydrographique"),
        UNH.normale = sum(
          UNH_message == "Unite hydrographique correspondante"))

    # Create tables
    commentaire <- c(
      "Alertes POP mauvaises unite hydrographique")
    table_stat <- make_tab(x = UNH_stat,
                           colours = c("#BF3111", "#BF3111",
                                       "#BF3111", "#5AB5BF"),
                           titre = "Rapport unite hydrographique (%)",
                           comment = commentaire)
    table_nb <- make_tab(x = UNH_nb,
                         colours = c("#BF3111", "#BF3111",
                                     "#BF3111", "#5AB5BF"),
                         titre = "Rapport unite hydrographique (nb)",
                         comment = commentaire)
  }

  # create pdf files
  if (pdf == TRUE) {
    grDevices::pdf(file = paste0(dir, "/", file.name, ".pdf"))
    if (stat == TRUE) {
      grid::grid.newpage()
      grid::grid.draw(table_stat) # percent
      grid::grid.newpage()
      grid::grid.draw(table_nb) # nb
    }
    if (stat == FALSE & stat == FALSE) {
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
    utils::write.csv(data_UNH, paste0(dir, "/", file.name, ".csv"), row.names = FALSE)
  } else {
    print("no csv written ==> write csv = TRUE to see it")
  }
  return(data_UNH)
}

