#' create_cohort
#'
#' This function allows the user to create a cohort for analysis based on cancer diagnosis information such as cancer cohort, treating institution, histology, stage at diagnosis, as well as cancer-directed regimen information including regimen name and regimen order.
#' This will return two datasets: (1) cohort_ca_dx will contain all patients matching the cancer diagnosis criteria of interest; (2) cohort_ca_drugs will return the drug-regimen data to those patients.
#'
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC" or "CRC".
#' @param institution GENIE BPC participating institution. Must be one of "DFCI", "MSK", "UHN", or "VICC" for NSCLC cohorts; must be one of "DFCI", "MSK", "VICC" for CRC. Default is all institutions.
#' @param stage_dx Stage at diagnosis. Must be one of "Stage I", "Stage II", "Stage III", "Stage I-III NOS", "Stage IV". Default is all stages.
#' @param ca_hist_adeno_squamous Cancer histology. Must be one of "Adenocarcinoma", "Squamous cell", "Sarcoma", "Small cell carcinoma", "Other histologies/mixed tumor"
#' @param index_ca_dx Order of index cancer. Default is 1, indicating the first index cancer.
#' @param regimen_drugs Names of drugs in cancer-directed regimen, based on variable `regimen_drugs` in the ca_drugs dataset.
#' @param regimen_number Order of cancer-directed regimen. Default is 1, indicating the first time the regimen was received.
#' @param return_summary Specifies whether a summary table for the cohort is returned. Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
create_cohort <- function(cohort,
                          institution = c("DFCI", "MSK", "VICC", "UHN"),
                          stage_dx,
                          ca_hist_adeno_squamous,
                          index_ca_seq = 1,
                          regimen_drugs,
                          reg_line_one,
                          regimen_seq = 1,
                          return_summary = FALSE
                          ) {

  # apply to all variables (alt would be r language)
  cohort_temp <- cohort
  institution_temp <- institution

  # check params
  if (length(cohort_temp) > 1) {
    stop("Specify only one cohort at a time.")
  }

  if (!(cohort %in% c("NSCLC", "CRC"))) {
    stop("Select from available cancer cohorts: NSCLC, CRC")
  }

  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
                            distinct(stage_dx), stage_dx)
  }
  else {
    stage_dx_temp <- {{stage_dx}}
  }

  # to account for unspecified histology
  if (missing(ca_hist_adeno_squamous)) {
    histology_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
                             distinct(ca_hist_adeno_squamous), ca_hist_adeno_squamous)
  }
  else {
    histology_temp <- {{ca_hist_adeno_squamous}}
  }

  # select patients based on cohort, institution, stage at diagnosis, histology and cancer number
  cohort_ca_dx <- get(paste0("ca_dx_index_", cohort_temp)) %>%
    # renumber index cancer diagnoses
    group_by(cohort, record_id) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    # apply filter
    filter(institution %in% c(institution_temp),
    stage_dx %in% c(stage_dx_temp),
    ca_hist_adeno_squamous %in% c(histology_temp),
    index_ca_seq %in% c({{index_ca_seq}})
    )

  # of those patients, if applicable, pull relevant drug regimens
  # all drug regimens to all patients in cohort
  if (missing(regimen_drugs) & missing(regimen_seq) & missing(reg_line_one)) {
  cohort_ca_drugs <- left_join(cohort_ca_dx,
                               get(paste0("ca_drugs_", cohort_temp)),
                               by = c("cohort", "record_id", "institution", "ca_seq"))
  }


  # first line regimen to all pts in cohort, any regimen
  if (missing(regimen_drugs) & missing(regimen_seq) & reg_line_one == TRUE) {
    cohort_ca_drugs <- left_join(cohort_ca_dx,
                                 get(paste0("ca_drugs_", cohort_temp)),
                                 by = c("cohort", "record_id", "institution", "ca_seq"))
  }

  # option 2: if specific drug regimen is requested
  # all times that drug regimen was received
  if (!missing(regimen_drugs) & missing(regimen_seq) & missing(reg_line_one)) {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c({{regimen_drugs}})) %>%
      # define number of times that regimen was received
      group_by(cohort, record_id) %>%
      mutate(regimen_seq = 1:n()) %>%
      ungroup()


    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 select(cohort, record_id, institution, ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq"))
  }

  # 1st (or other) time that regimen was received
  else if (!missing(regimen_drugs) & !missing(regimen_seq) & missing(reg_line_one)) {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c({{regimen_drugs}})) %>%
      # define number of times that regimen was received
      group_by(cohort, record_id) %>%
      mutate(regimen_seq = 1:n()) %>%
      ungroup() %>%
      # filter on order of interest (e.g. first, all)
      filter(regimen_seq == {{regimen_seq}})


    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 select(cohort, record_id, institution, ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq"))
  }

  # specific first line regimens
  else if (!missing(regimen_drugs) & missing(regimen_seq) & reg_line_one == TRUE) {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c({{regimen_drugs}})) %>%
      # define number of times that regimen was received
      group_by(cohort, record_id) %>%
      slice(which.min(regimen_number))

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
