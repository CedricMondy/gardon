# to create the documentation
# devtools::document()

# to create a vignette
# usethis::use_vignette("gardon-vignette")



# ===========================

#' Resume les resultats des test
#' @description Fusion des resultats des test realisees au sein d'un seul fichier et production de graphes resumant ces resultats.
#'
#' @param data Donnees de sortie de la fonction \code{\link{getDataRegion}}. Ou donnees en exemple : `data = exemple_IDF`.
#' @param dir.in Chemin d'acces ou se trouve les fichiers csv qui correspondent aux sorties des tests realisees.
#' @param dir.out Chemin d'acces ou enregistrer les fichiers de sortie.
#' @param file.name Nom a donner aux fichiers en sortie. Par defaut `file.name = "final_stat"`.
#' @param color.alerte Vecteur. Contient les deux couleurs indiquant une alerte ou non. Par defaut, `color.alerte = pal_alerte`.
#' @param color.type Vecteur. Contient les couleurs des types d'alertes. Par defaut, `color.type = divergent_24`.
#' @param option Deux possibilites pour charger les tableaux des resultats des tests. Si `option = 1` les fichiers .csv sont importes depuis le repertoire de travail indique (`dir.in`) dans lequel les resultats des fonctions ont ete enregiste. Si `option = 2`, `file.list = c("")` doit etre complete avec les noms des dataframes presentes dans l'environnement de R.
#' @param file.list Si `option = 2`, `file.list = c("")` doit etre complete avec les noms des dataframes presentes dans l'environnement de R correspondant aux resultats des fonctions.
#' @param pdf Argument logique `TRUE/FALSE`. Par defaut `pdf = TRUE` et un pdf avec les figures est cree dans le repertoire indique (`dir`).
#' @param plot Argument logique `TRUE/FALSE`. Par defaut `plot = TRUE` et un boxplot de la dispertion des valeurs est trace.
#' @param csv Argument logique `TRUE/FALSE`. Par defaut `csv = TRUE` et le fichier csv est enregistre dans le repertoire de travail ou le repertoire indique (`dir`).

#' @return Des graphiques et un fichier csv. La table des alertes.
#' @export
#'
#' @details Attention ! Les resultats de certaines fonctions ne peuvent pas etre inseres dans cette fonction. Cela concerne les tests sur les especes et les lots : \code{\link{test_sp_occ}}, \code{\link{distri_sp}}, \code{\link{test_sp_effectif}}, \code{\link{test_ope_lotTyl}}.
#'
#' @examples
#' final_result <- join_Testresult(
#' data = data_IDF,
#' pdf = FALSE,
#' csv = FALSE,
#' option = 2,
#' file.list = c("result_Lar", "result_dist", "result_DS", "result_TempJanv",
#'               "result_profondeur", "result_longueur")
#'               )
join_Testresult <- function(data,
                            plot = TRUE,
                            dir.in = NULL,
                            dir.out = NULL,
                            file.name = "final_stat",
                            color.alerte = pal_alerte,
                            color_type = divergent_24,
                            option = 2,
                            file.list = NULL,
                            csv = FALSE,
                            pdf = FALSE) {

  if (option == 2 & is.null(file.list) == TRUE) {
    stop("Si option = 2 alors file.list doit etre renseigne de la forme
         file.list = c('','')")
  } else if (option == 1 & (is.null(dir.in) == TRUE |
                            is.null(dir.out == TRUE))) {
    stop("Si option = 1 alors dir.in et dir.out doivent etre renseigne")
  }

  # Simplify data
  data_simple <- data %>%
    dplyr::select(ope_id,
                  sta_code_sandre,
                  pop_id,
                  ope_jour,
                  enh_libelle_sandre) %>%
    unique()

  # Load sortie de test ".csv"
  if (option == 1) {
    # pour les donnees sta, pop, ope
    setwd(dir.in)
    temp <- list.files(pattern = "*.csv")
    myfiles <- lapply(temp, read_delim)
  } else if (option == 2) {
    myfiles <- lapply(file.list, get)
  }

  # delete column of certaines dataframe
  myfiles <- lapply(
    myfiles,
    function(x) {
      delete_cols <- c("lop_id",
                       "lop_effectif",
                       "lop_tyl_id",
                       "lop_longueur_specimens_taille_maxi",
                       "lop_longueur_specimens_taille_mini",
                       "lop_poids",
                       "lotTyl_lotmessage",
                       "lotTyl_lottype",
                       "esp_code_alternatif",
                       "esp_nom_commun",
                       "effectif_spmessage",
                       "effectif_sptype",
                       "ecart_speffectif",
                       "sum_effectif",
                       "median_pop",
                       "esp_nom_latin",
                       "liste_spmessage",
                       "liste_sptype",
                       "freq",
                       "occ_spmessage",
                       "occ_sptype",
                       "fac_profondeur_moyenne",
                       "fac_importance_relative",
                       "fac_tyf_id"
      )
      x[delete_cols] <- NULL; x
    }
  )
  myfiles <- lapply(myfiles, unique)

  # Join them with the original data
  for (i in myfiles) {
    if (any(grepl("sta_code_sandre", colnames(i))) == TRUE) {
      data_simple <- dplyr::left_join(data_simple, i)

    } else if (any(grepl("pop_id", colnames(i))) == TRUE) {
      data_simple <- dplyr::left_join(data_simple, i)

    } else if (any(grepl("ope_id", colnames(i))) == TRUE) {
      data_simple <- dplyr::left_join(data_simple, i)
    }
  }

  # Create a general column indicating if a operation is an alert
  final_report <- data_simple %>%
    dplyr::select(
      ope_id, dplyr::contains("_type")
    ) %>%
    tidyr::pivot_longer(
      cols = -ope_id
    ) %>%
    dplyr::group_by(ope_id, name) %>%
    dplyr::count(value, name = "nb_alerte") %>%
    dplyr::filter(value == "alerte") %>%
    # dplyr::summarise(
    #   any_alert = any(stringr::str_detect(value, "alerte"))
    #   ) %>%
    dplyr::left_join(data_simple, ., by = "ope_id")
  if (sum(final_report$nb_alerte, na.rm = TRUE) == 0) {
    print("Aucune alerte detectee")
    return(final_report)
  }

  # Make a bar plot with number of alertes per test
  if (plot == TRUE) {
    g <- final_report %>%
      dplyr::select(ope_id, contains("_type")) %>%
      unique() %>%
      tidyr::pivot_longer(cols = -ope_id, names_to = "parameters",
                          values_to = "values") %>%
      dplyr::group_by(parameters) %>%
      dplyr::count(values) %>%
      ggplot2::ggplot(aes(x = parameters, y = n, fill = values)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = pal_alerte) +
      ggplot2::ggtitle("Nombre d'operation en alerte par test") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Type de test", y = "Nombre d'operations",
                    fill = "Statue")
    print(g)

    # Make a bar plot with number of alertes per cours eau
    ope_cours_eau <- final_report %>%
      dplyr::group_by(enh_libelle_sandre) %>%
      dplyr::summarise(nb_ope = dplyr::n_distinct(ope_id))

    g1 <- final_report %>%
      dplyr::select(ope_id, enh_libelle_sandre, nb_alerte, name) %>%
      unique() %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(aes(x = enh_libelle_sandre,
                            y = nb_alerte, fill = name)) +
      # ggplot2::geom_point(data = ope_cours_eau, aes(x = enh_libelle_sandre,
      #                                               y = nb_ope)) +
      ggplot2::scale_fill_manual(values = color_type) +
      ggplot2::ggtitle("Nombre d'operation en alerte par cours eau") +
      ggplot2::labs(x = "Cours eau", y = "Nombre d'alertes",
                    fill = "Types test") +
      ggplot2::coord_flip()
    print(g1)

    # Make a bar plot with number of alertes per year
    ope_annee <- final_report %>%
      dplyr::mutate(annee = format(ope_jour, format = "%Y")) %>%
      dplyr::group_by(annee) %>%
      dplyr::summarise(nb_ope = dplyr::n_distinct(ope_id))

    g2 <- final_report %>%
      dplyr::mutate(annee = format(ope_jour, format = "%Y")) %>%
      dplyr::select(ope_id, annee, nb_alerte, name) %>%
      unique() %>%
      ggplot2::ggplot(aes(x = as.factor(annee), y = nb_alerte, fill = name)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = color_type) +
      ggplot2::ggtitle("Nombre d'operation en alerte par annee") +
      ggplot2::labs(x = "Annees", y = "Nombre d'alertes", fill = "Types test") +
      ggplot2::coord_flip()
    print(g2)

    # g3 <- final_report %>%
    #   dplyr::mutate(annee = format(ope_jour, format = "%Y")) %>%
    #   dplyr::select(ope_id, annee, nb_alerte, name) %>%
    #   unique() %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_col(aes(x = as.factor(annee), y = nb_alerte, fill = name)) +
    #   ggplot2::geom_point(data = ope_annee, aes(x = as.factor(annee),
    #                                             y = nb_ope)) +
    #   ggplot2::scale_fill_manual(values = color_type) +
    #   ggplot2::ggtitle("Nombre d'operation en alerte par annee") +
    #   ggplot2::labs(x = "Annees", y = "Nombre d'alertes", fill = "Types test") +
    #   ggplot2::coord_flip()
    # print(g3)

  }

  # Create pdf
  if (pdf == TRUE & plot == TRUE) {
    grDevices::pdf(file = paste0(dir.in, "/", file.name, ".pdf"))
    print(g) # boxplot
    print(g1) # par cours eau
    print(g2)
    grDevices::dev.off()
  }

  # Save in .csv
  if (csv == TRUE) {
    utils::write.csv(final_report,
                     paste0(dir.out, "/", file.name, ".csv"),
                     row.names = FALSE)
  }

  return(final_report)

}
