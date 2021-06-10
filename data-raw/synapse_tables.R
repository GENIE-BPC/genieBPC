#' Synapse table IDs
#'
#' A dataset containing the Synapse table IDs for each clinical dataset in GENIE BPC.
#'
#' @format A lookup table for Synapse clinical data table IDs:
#' \describe{
#'   \item{cohort}{GENIE BPC Project Cohort}
#'   \item{df}{Clinical dataset}
#'   \item{version}{Release version}
#'   \item{synapse_id}{Synapse table ID for each dataset}
#'   ...
#' }
#' @source \url{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}
synapse_tables <- tidyr::tibble(
  cohort = c(rep("NSCLC", 8), rep("CRC", 9)),
  df = c("pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs", "prissmm_pathology", "prissmm_imaging", "prissmm_md", "cpt",
  "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs", "prissmm_pathology", "prissmm_imaging", "prissmm_md", "tm", "cpt"),
  version = c(rep("v1.1", 17)),
  synapse_id = c("syn22418979", "syn22418974", "syn22418975", "syn22418980", "syn22418982", "syn22418981", "syn22418986", "syn22418987",
                 "syn24168397", "syn24168395", "syn24168396", "syn24168398", "syn24168400", "syn24168399", "syn24168401", "syn24168403", "syn24168402"
                 )
)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
