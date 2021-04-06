#' Obtain clinical data for GENIE BPC from Synapse
#'
#' The `pull_data_synapse` function access the specified version of the clinical GENIE BPC data from \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse} and reads it into the R environment.
#'
#' To obtain access to Synapse or the GENIE BPC project ...
#'
#' @param cohort Specify the cohort of interest (NSCLC or CRC)
#' @param version Specify the version of the data. By default, the most recent version is pulled. Currently only version 1.1 is available.
#' @export
#' @import
#' dplyr
#' dtplyr
#' tibble
#' synapser
pull_data_synapse <- function(cohort, version = "1.1") {
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
    version <- c("1.1")
    print("v1.1 selected by default.")
  }

  # pull data from synapse
  # NSCLC v1.1
  if (cohort == "NSCLC" & version == "1.1") {
    pt_char <- read.csv(synGet("syn22418979")$path)
    ca_dx_index <- read.csv(synGet("syn22418974")$path)
    ca_dx_non_index <- read.csv(synGet("syn22418975")$path)
    ca_drugs <- read.csv(synGet("syn22418980")$path)
    prissmm_pathology <- read.csv(synGet("syn22418982")$path)
    prissmm_imaging <- read.csv(synGet("syn22418981")$path)
    prissmm_md <- read.csv(synGet("syn22418986")$path)
    cpt <- read.csv(synGet("syn22418987")$path)
  }

  # CRC v1.1
  if (cohort == "CRC" & version == "1.1") {
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

  return(list("pt_char" = pt_char,
              "ca_dx_index" = ca_dx_index,
              "ca_dx_non_index" = ca_dx_non_index,
              "ca_drugs" = ca_drugs,
              "prissmm_pathology" = prissmm_pathology,
              "prissmm_imaging" = prissmm_imaging,
              "prissmm_md" = prissmm_md,
              #"prissmm_tm" = prissmm_tm, # only returned if not NSCLC
              "cpt" = cpt))
}
