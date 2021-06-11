#' synapse_version
#'
#' Data is updated periodically and each time it is updated the cohort will get a new version number.
#' This function will get each version number for each cohort and will help you determine what is the most recent version for each cohort.
#'
#' The `synapse_version` function access the specified version of the clinical GENIE BPC data from \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse} and reads it into the R environment.
#'
#' To obtain version numbers and pull the correct one for each cohort
#'
#' @param most_recent TRUE/FALSE indicator if  you only want the most recent version number for each cohort
#' @export
#' @import
#'
#' dplyr

synapse_version <- function(most_recent = FALSE){
  if(!is.logical(most_recent)){
    stop("Please provide TRUE or FALSE for the most_recent argument")
  }
  if(is.na(most_recent)){
    stop("Please provide TRUE or FALSE for the most_recent argument")
  }

  if (most_recent == FALSE){
    synapse_tables %>%
      group_by(cohort, version) %>%
      filter(row_number()==1) %>%
      select(cohort,version) %>%
      mutate(all_versions = "All Versions")
  }
  else{
    synapse_tables %>%
      arrange(cohort, desc(version)) %>%
      group_by(cohort) %>%
      filter(row_number()==1) %>%
      select(cohort,version) %>%
      mutate(all_versions = "Most Recent Versions")

  }

}
