#' pull_data_synapse
#'
#' Pull data from synapse
#' @param cohort Specify the cohort of data to pull (NSCLC or CRC)
#' @param version Specify which version of the data to pull. By default, the most recent version is pulled.
#' @export
#' @import
#' dplyr
#' dtplyr
#' tibble
#' synapser
pull_data_synapse <- function(cohort, version) {
  synapser::synLogin()

  # check params
  if (is.null(cohort)) {
    cohort <- c("NSCLC", "CRC")
    print("Data for all available cohorts is being pulled (NSCLC, CRC)")
  }
  if (!(cohort %in% c("NSCLC", "CRC"))) {
    stop("Select from NSCLC or CRC cohorts.")
  }
  if (is.null(version)) {
    version <- c("v1.1")
    print("v1.1 selected by default.")
  }


  # pull data from synapse
  # CRC v1.1
  pt_char <- read.csv(synGet("syn24168397")$path)
  ca_dx_index <- read.csv(synGet("syn24168395")$path)
  ca_dx_non_index <- read.csv(synGet("syn24168396")$path)
  ca_drugs <- read.csv(synGet("syn24168398")$path)
  prissmm_pathology <- read.csv(synGet("syn24168400")$path)
  prissmm_imaging <- read.csv(synGet("syn24168399")$path)
  prissmm_md <- read.csv(synGet("syn24168401")$path)
  prissmm_tm <- read.csv(synGet("syn24168403")$path)
  cpt <- read.csv(synGet("syn24168402")$path)
}
