#' Return list of available GENIE BPC data releases
#'
#' GENIE BPC data are updated periodically to add variables and reflect
#' additional data cleaning. Each time the data are updated the data release
#' version number is incremented. The `synapse_version()` function will get
#' available version numbers for each cohort to help the user determine what is
#' the most recent version for each cohort.
#'
#' Specifies the version numbers available for each cancer cohort. Version
#' numbers are specified as part of the call to `pull_data_synapse()`.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest. Must be
#'   one of "NSCLC" (Non-Small Cell Lung Cancer), "CRC" (Colorectal Cancer), or
#'   "BrCa" (Breast Cancer), "PANC" (Pancreatic Cancer), "Prostate" (Prostate Cancer),
#'   and "BLADDER" (Bladder Cancer).
#' @param most_recent Indicates whether the function will return only the most
#'   recent version number for each cohort (`most_recent` = TRUE) or all
#'   available version numbers for each cohort (`most_recent`= FALSE)
#' @return Returns a table containing the available versions for each cohort.
#'   Consortium releases are restricted to GENIE BPC consortium members.
#' @examples
#' synapse_version()
#' synapse_version(most_recent = TRUE)
#' @export
#' @import
#'
#' dplyr

synapse_version <- function(cohort = NULL, most_recent = FALSE) {
  if (!is.logical(most_recent)) {
    stop("Please provide TRUE or FALSE for the most_recent argument")
  }

  # if cohort is not specified, return all cohorts
  if (is.null(cohort)){
    select_cohort <- c("NSCLC", "CRC", "BrCa", "BLADDER", "PANC", "Prostate")
  } else {
    select_cohort <- rlang::arg_match(cohort, c("NSCLC", "CRC", "BrCa", "BLADDER", "PANC", "Prostate"),
                                    multiple = TRUE)
  }

  # define numeric release date
  synapse_tables_dts <- genieBPC::synapse_tables %>%
    mutate(pubcon = stringr::str_extract(version,'\\b\\w+$'),
           numeric_release_date =
             as.Date(paste0(as.numeric(word(.data$release_date, 1, sep = "-")),
                            "-",
                            as.numeric(word(.data$release_date, 2, sep = "-")),
                            "-01"),
                     format = "%Y-%m-%d"))

  if (most_recent == FALSE) {
    synapse_tables_dts %>%
      group_by(.data$cohort, .data$version) %>%
      filter(.data$cohort %in% c(select_cohort),
             row_number() == 1) %>%
      ungroup() %>%
      arrange(.data$cohort, .data$numeric_release_date) %>%
      select("cohort", "version", "release_date") %>%
      mutate(versions_returned = "All Versions")
  } else {
    synapse_tables_dts %>%
      filter(.data$cohort %in% c(select_cohort)) %>%
      group_by(.data$cohort, .data$pubcon) %>%
      slice(which.max(.data$numeric_release_date)) %>%
      ungroup() %>%
      arrange(.data$cohort, .data$numeric_release_date) %>%
      select("cohort", "version", "release_date") %>%
      mutate(versions_returned = "Most Recent Versions")
  }
}
