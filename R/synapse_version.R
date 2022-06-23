#' synapse_version
#'
#' GENIE BPC data are updated periodically to add variables and reflect additional data cleaning.
#' Each time the data are
#' updated the data release version number is incremented.
#' The `synapse_version()` function will get available version numbers for each
#' cohort to help the user determine what is the most
#' recent version for each cohort.
#'
#' The `synapse_version` function access the specified
#' version of the clinical GENIE BPC data from
#' \href{https://www.synapse.org/#!Synapse:syn27056172/wiki/616601}{Synapse}
#' and reads it into the R environment.
#'
#' To obtain version numbers and pull the correct version for each cohort
#'
#' @param most_recent TRUE/FALSE indicator if  you only want the most recent
#' version number for each cohort
#' @return Returns a table containing the available versions for each cohort.
#' Consortium releases are restricted to GENIE BPC consortium members.
#' @examples
#' synapse_version()
#' synapse_version(most_recent = TRUE)
#' @export
#' @import
#'
#' dplyr

synapse_version <- function(most_recent = FALSE) {
  if (!is.logical(most_recent)) {
    stop("Please provide TRUE or FALSE for the most_recent argument")
  }

  if (most_recent == FALSE) {
    genieBPC::synapse_tables %>%
      group_by(.data$cohort, .data$version) %>%
      filter(row_number() == 1) %>%
      select(.data$cohort, .data$version) %>%
      mutate(versions_returned = "All Versions")
  } else {
    genieBPC::synapse_tables %>%
      mutate(pubcon = stringr::str_extract(version,'\\b\\w+$'),
             numeric_release_date =
               as.Date(paste0(as.numeric(word(.data$release_date, 1, sep = "-")),
                              "-",
                              as.numeric(word(.data$release_date, 2, sep = "-")),
                              "-01"),
                       format = "%Y-%m-%d")) %>%
      group_by(.data$cohort, .data$pubcon) %>%
      slice(which.max(.data$numeric_release_date)) %>%
      select(.data$cohort, .data$version) %>%
      # mutate(version = substr(.data$version, 2, 4)) %>%
      mutate(versions_returned = "Most Recent Versions")
  }
}
