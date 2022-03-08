#' Obtain Clinical Data Files for GENIE BPC Project
#'
#' The `pull_data_synapse` function accesses the specified
#'  version of the clinical and genomic GENIE BPC data from
#'  \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse}
#'  and reads it into the R environment.
#'   Documentation corresponding to the clinical data files
#'   can also be found on Synapse in the Analytic Data Guide:
#' \itemize{
#'   \item \href{https://www.synapse.org/#!Synapse:syn23002641}{NSCLC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26008058}{NSCLC v2.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn23764204}{CRC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26077308}{CRC v1.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26077313}{BrCa v1.1-Consortium Analytic Data Guide}
#' }
#' Users must log in to Synapse to access the data successfully.
#' To log into Synapse during each session, call:
#' `synLogin(email = "your_email", password = "your_password")`
#' To store authentication information in your operating system, call:
#' `synLogin(email="your_email",password="your_password",rememberMe=TRUE)`
#' Upon calling the `rememberMe = TRUE` argument, the user can call:
#' `synLogin()` in future uses without specifying login credentials.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest.
#'  Must be one of "NSCLC" (Non-Small Cell Lung Cancer) or
#'   "CRC" (Colorectal Cancer).
#' @param version Vector or list specifying the version of the data.
#' By default, the most recent version is pulled.
#' Currently, only version 1.1 is available.
#' When entering multiple cohorts, the order of the
#' version numbers corresponds to the order that the cohorts
#' are specified; the cohort and version number must be in
#' the same order in order to pull the correct data. See examples below.
#'
#' @return Returns clinical and genomic data corresponding to
#' the specified cohort(s). Data frames have the suffix
#' indicating the cohort appended to their name,
#' e.g. pt_char_NSCLC for the pt_char dataset of
#' the NSCLC cohort.
#' @author Michael Curry
#' @export
#'
#' @examples
#' if(genieBPC:::check_synapse_login() == FALSE){
#' # Example 1 ----------------------------------
#' # Pull non-small cell lung cancer data
#'
#'  pull_data_synapse(cohort = "NSCLC", version = "2.1-consortium")
#'
#' # Example 2 ----------------------------------
#' # Pull the most recent non-small cell lung cancer
#' # data and the most recent colorectal cancer data
#'  pull_data_synapse(cohort = c("NSCLC", "CRC"),
#'  version = c("2.1-consortium", "1.2-consortium"))
#'
#' # Example 3 ----------------------------------
#' # Pull version 2.1 for non-small cell lung cancer
#'  #and version 1.1 for colorectal cancer data
#'  pull_data_synapse(
#'   cohort = c("NSCLC", "CRC"),
#'   version = c("2.1-consortium", "1.1-consortium")
#'  )
#' }
#' @import
#' dplyr
#' dtplyr


pull_data_synapse <- function(cohort, version) {
  if("synapser" %in% rownames(utils::installed.packages()) == FALSE) {
    #install.packages("synapser", repos = "http://ran.synapse.org")
    stop("Please install the package synapser from http://ran.synapse.org")
  }
  tryCatch(
    synapser::synLogin(),
    error =
      function(e) {
        cli::cli_alert_warning(
          paste("There was an error pulling the data.",
                "See error message below."))
        paste("You are not logged into your synapse account",
              "Please set credentials by using 'synapser::synLogin()'",
              "To store login credentials in your operating system, call:",
              "`synLogin(email, password, rememberMe = TRUE)`",
              "Note: Upon calling `rememberMe = TRUE`, the user can call:",
              "`synLogin()` in future uses without specifying credentials.",
              sep = "\n"
        ) %>%
          stop(call. = FALSE)
      }
  )
  tryCatch(
    {
      # check parameters
      if (missing(cohort)) {
        stop("Select cohort from 'NSCLC', 'CRC' or 'BrCa'")
      }
      if (sum(!grepl("^CRC$|^NSCLC$|^BRCA$", stringr::str_to_upper(cohort)))
          > 0) {
        stop("Select cohort from 'NSCLC', 'CRC' or 'BrCa")
      }
      if (missing(version)) {
        stop("Version needs to be specified. Use `synapse_version()` to see
             what data is available.")
      }
      if(length(cohort) < length(version)){
        stop("You have selected more versions than cancer cohorts.
             Make sure cohort and version inputs have the same length")
      }

      versionnum <- dplyr::distinct(synapse_tables,cohort, version)

      versionnum <- dplyr::mutate(versionnum,
                                  version = substr(version,2,nchar(version)),
                                  cohort = toupper(.data$cohort))

      cohortval <- toupper(cohort)

      versionnum <- dplyr::filter(versionnum, cohort %in% cohortval)

      if( !all(version %in% unique(versionnum$version))){
        stop("You have selected a version that is not
        available for this cohort. Please use `synapse_tables`
             to see what versions are available.")
      }
      # get lists of available versions for Synapse tables and
      # corresponding file names, appended with cohort name
      synapse_tables$version <- substr(
        synapse_tables$version, 2,
        nchar(synapse_tables$version))
      synapse_tables$filenames <- paste(
        synapse_tables$df, synapse_tables$cohort, sep = "_")

      cohort_version <- Map(
        function(x, y) {
          synapse_tables[stringr::str_to_upper(
            synapse_tables$cohort) %in% x & synapse_tables$version %in% y, ]
        },
        stringr::str_to_upper(cohort),
        version
      )

      synapse_tables2 <- do.call(rbind, cohort_version)

      synapse_tables2$path <- vapply(seq_along(
        as.data.frame(synapse_tables2)[,1]), function(x) {
        synapser::synGet(synapse_tables2$synapse_id[x])$path
      }, character(1))

      # read Synapse tables
      readcsvfile <- stats::setNames(
        lapply(synapse_tables2$path[grepl(".csv", synapse_tables2$path)],
               utils::read.csv),
        synapse_tables2$filenames[grepl(".csv", synapse_tables2$path)]
      )



      readtxtfile <- stats::setNames(
        lapply(synapse_tables2$path[grepl(".txt", synapse_tables2$path)],
               function(x) {
          utils::read.delim(x, sep = "\t")
        }),
        synapse_tables2$filenames[grepl(".txt", synapse_tables2$path)]
      )

      readfiles <- c(readcsvfile, readtxtfile)



      # return Synapse tables to list
      return(readfiles)
    },

    # return error messages
    error = function(e) {
      cli::cli_alert_warning(paste("There was an error pulling the data.",
      "See error message below."))
      stop(e)
    }
  )
}
