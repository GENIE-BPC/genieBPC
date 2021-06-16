#' Obtain Clinical Data Files for GENIE BPC Project
#'
#' The `pull_data_synapse` function accesses the specified version of the clinical GENIE BPC data from \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse} and reads it into the R environment. Documentation corresponding to the clinical data files can also be found on Synapse in the Analytic Data Guide.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest. Must be one of "NSCLC" (Non-Small Cell Lung Cancer) or "CRC" (Colorectal Cancer).
#' @param version Vector or list specifying the version of the data. By default, the most recent version is pulled. Currently, only version 1.1 is available. When entering multiple cohorts, the order of the version numbers corresponds to the order that the cohorts are specified; the cohort and version number must be in the same order in order to pull the correct data. See examples below.
#'
#' @return Returns clinical data corresponding to the specified cohort(s). Data frames have the suffix indicating the cohort appended to their name, e.g. pt_char_NSCLC for the pt_char dataset of the NSCLC cohort.
#' @author Mike Curry
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' # Pull the most recent non-small cell lung cancer data
#' pull_data_synapse(cohort = "NSCLC")
#'
#' # Example 2 ----------------------------------
#' # Pull the most recent non-small cell lung cancer data and the most recent colorectal cancer data
#' pull_data_synapse(cohort = c("NSCLC", "CRC"))
#'
#' # Example 3 ----------------------------------
#' # Pull version 2.1 for non-small cell lung cancer and version 1.1 for colorectal cancer data
#' pull_data_synapse(
#'   cohort = c("NSCLC", "CRC"),
#'   version = c("2.1", "1.1")
#' )
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
          sep = "\n"
        ) %>%
          stop(call. = FALSE)
      }
  )
  tryCatch(
    {
      # check parameters
      if (missing(cohort)) {
        stop("Select cohort from 'NSCLC' or 'CRC'")
      }
      if (sum(!grepl("^CRC$|^NSCLC$", cohort)) > 0) {
        stop("Select cohort from 'NSCLC' or 'CRC'")
      }
      if (missing(version)) {
        print("Version '1.1' selected by default.")
      }

      # get lists of available versions for Synapse tables and corresponding file names, appended with cohort name
      synapse_tables$version <- substr(synapse_tables$version, 2, nchar(synapse_tables$version))
      synapse_tables$filenames <- paste(synapse_tables$df, synapse_tables$cohort, sep = "_")

      cohort_version <- Map(
        function(x, y) {
          synapse_tables[synapse_tables$cohort %in% x & synapse_tables$version %in% y, ]
        },
        cohort,
        version
      )

      synapse_tables2 <- do.call(rbind, cohort_version)

      # read Synapse tables
      readfiles <- lapply(1:nrow(synapse_tables2), function(x) {
        utils::read.csv(synapser::synGet(synapse_tables2$synapse_id[x])$path)
      })

      # name Synapse tables
      names(readfiles) <- synapse_tables2$filenames

      # return Synapse tables to list
      return(readfiles)
    },

    # return error messages
    error = function(e) {
      cli::cli_alert_warning("There was an error pulling the data. See error message below.")
      stop(e)
    }
  )
}
