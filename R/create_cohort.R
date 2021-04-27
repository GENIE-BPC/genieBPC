#' create_cohort
#'
#' This function allows the user to create a relevant cohort for analysis based on cancer diagnosis information such as cancer cohort, treating institution, histology, stage at diagnosis, as well as cancer-directed regimen information including regimen name and regimen order.
#' This will return a dataset (`cohort_ca_dx`) containing all patients matching the criterion of interest. If drug regimen criteria are also specified, then a second dataset (`cohort_ca_drugs`) will also be returned.
#'
#' @param cohort GENIE BPC Project cancer
#' @param institution GENIE BPC participating institution
#' @param stage_dx Stage at diagnosis
#' @param ca_hist_adeno_squamous Cancer histology
#' @param index_ca_dx Order of index cancer (first index cancer or all index cancers)
#' @param regimen_drugs Names of drugs in cancer-directed regimen
#' @param regimen_number Order of cancer-directed regimens returned (first time a regimen was received or all times a regimen was received)
#' @param return_summary Specifies whether or not a summary table for the cohort is returned
#'
#' @return
#' @export
#'
#' @examples
create_cohort <- function(cohort = c("NSCLC", "CRC"),
                          institution = c("DFCI", "MSK", "VICC", "UHN"),
                          stage_dx,
                          ca_hist_adeno_squamous,
                          index_ca_dx = "first",
                          regimen_drugs,
                          regimen_number = "first",
                          return_summary = FALSE
                          ) {

  # apply to all variables (alt would be r language)
  cohort_temp <- cohort
  institution_temp <- institution
  regimen_drugs_temp <- regimen_drugs

  # check params
  if (max(grepl(cohort_temp, pull(ca_dx_index, cohort))) == 0 ) {
    print("The current data in your environment does not include the cancer cohort you are trying to build. Try to run `pull_synapse` with the current cohort of interest.")
  }

  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(ca_dx_index %>% distinct(stage_dx), stage_dx)
  }
  else {
    stage_dx_temp <- {{stage_dx}}
  }

  # to account for unspecified histology
  if (missing(ca_hist_adeno_squamous)) {
    histology_temp <- pull(ca_dx_index %>% distinct(ca_hist_adeno_squamous), ca_hist_adeno_squamous)
  }
  else {
    histology_temp <- {{ca_hist_adeno_squamous}}
  }

  # select patients based on cohort, institution, stage at diagnosis, histology and cancer number
  cohort_ca_dx <- ca_dx_index %>%
    filter(cohort %in% c(cohort_temp),
           institution %in% c(institution_temp),
           stage_dx %in% c(stage_dx_temp),
           ca_hist_adeno_squamous %in% c(histology_temp)
           )

  # if only the first index cancer is requested, subset the cohort dataset
  if (index_ca_dx == "first") {
    cohort_ca_dx <- cohort_ca_dx %>%
      group_by(cohort, record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup()
  }

  # of those patients, if applicable, pull relevant drug regimens
  # option 1: get all drugs to patients in this cohort
  cohort_ca_drugs <- left_join(cohort_ca_dx,
                               ca_drugs,
                               by = c("cohort", "record_id", "institution", "ca_seq"))

  # option 2: specific drug regimen is requested
  if (!missing(regimen_drugs_temp)) {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c(regimen_drugs_temp))

    # if only the first time that regimen appears is of interest
    # (as opposed to any receipt of that regimen)
    if (regimen_number == "first") {
      cohort_ca_drugs <- cohort_ca_drugs %>%
        group_by(cohort, record_id, ca_seq, regimen_drugs) %>%
        slice(which.min(regimen_number)) %>%
        ungroup()
    }

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 select(cohort, record_id, institution, ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq"))
      }

  # return a table 1 to describe the cancer cohort if the user specifies
  if (return_summary == TRUE) {
  tbl1_cohort <- gtsummary::tbl_summary(cohort_ca_dx,
                             by = cohort,
                             include = c(cohort, institution, stage_dx, ca_hist_adeno_squamous),
                             )

  tbl_drugs <- gtsummary::tbl_summary(cohort_ca_drugs,
                                      by = cohort,
                                      include = c(cohort, institution, regimen_drugs))
  }

  return(list("cohort_ca_dx" = cohort_ca_dx,
              "cohort_ca_drugs" = cohort_ca_drugs,
              "tbl1_cohort" = tbl1_cohort,
              "tbl_drugs" = tbl_drugs
              ))
}
