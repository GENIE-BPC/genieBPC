#' Get each version number for each cohort. This function will help you determine what is the most recent version for each cohort.
#'
#' The `synapse_version` function access the specified version of the clinical GENIE BPC data from \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse} and reads it into the R environment.
#'
#' To obtain version numbers and pull the correct one for each cohort
#'
#' @param recent TRUE/FALSE indicator if  you only want the most recent version number for each cohort
#' @export
#' @import
#'
#' dplyr

synapse_version <- function(recent){


  synapse_tables %>%
  group_by(cohort, version) %>%
  filter(row_number()==1) %>%
  select(cohort,version)


}
