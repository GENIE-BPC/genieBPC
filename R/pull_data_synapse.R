#' Obtain clinical data for GENIE BPC from Synapse
#'
#' The `pull_data_synapse` function access the specified version of the clinical GENIE BPC data from \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse} and reads it into the R environment.
#' NOTE: when entering cohort and version number they must be in the same order in the lists in order to pull the correct data. See example below.
#' To obtain access to Synapse or the GENIE BPC project ...
#'
#' @param cohort Specify the cohort of interest (NSCLC or CRC)
#' @param version Specify the version of the data. By default, the most recent version is pulled. Currently only version 1.1 is available.
#' @export
#' @import
#' dplyr
#' dtplyr

pull_data_synapse <- function(cohort, version = "1.1") {
  tryCatch(
    synapser::synLogin(),
    error =
      function(e) {
        cli::cli_alert_warning("There was an error pulling the data. See error message below.")
        paste("You are not logged into your synapse account",
              "Please set credentials by using 'synapser::synLogin()'",
              sep ="\n") %>%
          stop(call. = FALSE)
      }
  )
  tryCatch({



    # check params
    if (missing(cohort)) {
      stop("Select from 'NSCLC' or 'CRC' cohorts.")
    }
    if (sum(!grepl("^CRC$|^NSCLC$", cohort))>0) {
      stop("Select from 'NSCLC' or 'CRC' cohorts.")
    }
    if (missing(version)) {
      print("Version '1.1' selected by default.")
    }

    synapse_tables$version <- substr(synapse_tables$version,2,nchar(synapse_tables$version))
    synapse_tables$filenames <- paste(synapse_tables$df, synapse_tables$cohort, sep = "_")

    cohort_version <- Map(function(x,y){
      synapse_tables[synapse_tables$cohort %in% x & synapse_tables$version %in% y,]},
      cohort,
      version)

    synapse_tables2 <- do.call(rbind, cohort_version)

    readfiles <-  lapply(1:nrow(synapse_tables2), function(x){
      read.csv(synGet(synapse_tables2$synapse_id[x])$path)

    })
    names(readfiles) <- synapse_tables2$filenames

    return(readfiles)
  },
  error = function(e) {
    cli::cli_alert_warning("There was an error pulling the data. See error message below.")
    stop(e)
  })
}


