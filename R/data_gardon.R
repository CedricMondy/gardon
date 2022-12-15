# to create the documentation
# devtools::document()

#' Couche cartographique des cours d'eau en Ile de France
#'
#'
#' @format ## `cours_eau`
#' Un sf object avec 2031 observations et 16 variables:
#' \describe{
#'   \item{ID}
#'   \item{CODE_HYDRO}
#'   \item{TOPONYME}
#'   \item{STATUT_TOP}
#'   \item{IMPORTANCE}
#'   \item{DATE_CREAT}
#'   \item{DATE_MAJ}
#'   \item{DATE_APP}
#'   \item{DATE_CONF}
#'   \item{SOURCE}
#'   \item{ID_SOURCE}
#'   \item{STATUT}
#'   \item{MAREE}
#'   \item{PERMANENT}
#'   \item{COMMENT}
#'   \item{geometry}
#' }
"cours_eau"

# ===========================

#' Donnees relatives aux points de prelevements
#'
#'
#' @format ## `point_prelevement`
#' Dataframe avec 138721 observations et 42 variables:
#' \describe{
#'   \item{pop_id}
#'   \item{pop_sta_id}
#'   \item{pop_code_sandre}
#'   \item{pop_code_wama}
#'   \item{pop_libelle_wama}
#'   \item{pop_com_code_insee_wama}
#'   \item{pop_enh_id}
#'   \item{pop_sus_id}
#'   \item{pop_geometrie}
#'   \item{pop_typ_id}
#'   \item{pop_coordonnees_x}
#'   \item{pop_coordonnees_y}
#'   \item{pop_lieu_dit}
#'   \item{pop_localisation_precise}
#'   \item{pop_distance_mer}
#'   \item{pop_distance_maree_dynamique}
#'   \item{pop_fog_id_cerema}
#'   \item{pop_ocs_id}
#'   \item{pop_zoh_id}
#'   \item{pop_reh_id_cerema}
#'   \item{pop_cap_id}
#'   \item{pop_acc_id}
#'   \item{pop_largeur_lit_mineur}
#'   \item{pop_unh_id}
#'   \item{pop_distance_source}
#'   \item{pop_altitude}
#'   \item{pop_pente_ign_cours_eau}
#'   \item{pop_surface_bassin_versant_amont}
#'   \item{pop_temperature_moyenne_janvier}
#'   \item{pop_temperature_moyenne_juillet}
#'   \item{pop_uti_id}
#'   \item{pop_date_derniere_modification}
#'   \item{pop}
#' }
"point_prelevement"

# ===========================
