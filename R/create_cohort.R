#' create_cohort
#'
#' This function allows the user to create a cohort for analysis based on cancer diagnosis information such as cancer cohort, treating institution, histology, stage at diagnosis, as well as cancer-directed regimen information including regimen name and regimen order.
#' This will return two datasets: (1) cohort_ca_dx will contain all patients matching the cancer diagnosis criteria of interest; (2) cohort_ca_drugs will return the drug-regimen data to those patients.
#'
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC" or "CRC".
#' @param institution GENIE BPC participating institution. Must be one of "DFCI", "MSK", "UHN", or "VICC" for NSCLC cohorts; must be one of "DFCI", "MSK", "VICC" for CRC. Default is all institutions.
#' @param stage_dx Stage at diagnosis. Must be one of "Stage I", "Stage II", "Stage III", "Stage I-III NOS", "Stage IV". Default is all stages.
#' @param ca_hist_adeno_squamous Cancer histology. Must be one of "Adenocarcinoma", "Squamous cell", "Sarcoma", "Small cell carcinoma", "Other histologies/mixed tumor"
#' @param index_ca_seq Index cancer sequence. Default is 1, indicating the patient's first index cancer.
#' @param regimen_drugs Names of drugs in cancer-directed regimen, based on variable `regimen_drugs` in the ca_drugs dataset.
#' @param regimen_order Order of cancer-directed regimen. Default is 1, indicating the first time the regimen was received. If multiple drugs are specified, `regimen_order` indicates the regimen order for all drugs; different values of `regimen_order` cannot be specified for different drug regimens.
#' @param regimen_order_type Specifies whether the `regimen_order` parameter refers to the order of receipt of the drug regimen within the cancer diagnosis (across all other drug regimens) or the order of receipt of the drug regimen within the times that that drug regimen was administered (e.g. the first time carboplatin pemetrexed was received, out of all times that the patient received carboplatin pemetrexed)
#' @param return_summary Specifies whether a summary table for the cohort is returned. Default is FALSE.
#'
#' @return cohort_ca_dx and cohort_ca_drugs data frames
#' @export
#'
#' @examples
#' Example1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC of histology adenocarcinoma
#' create_cohort(cohort = "NSCLC",
#'      stage_dx = c("Stage IV"),
#'      ca_hist_adeno_squamous = "Adenocarcinoma")
#' Example2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#'      create_cohort(cohort = "NSCLC",
#'      regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin, Etoposide"),
#'      regimen_order = 1,
#'      regimen_order_type = "within regimen")
#' Example3 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed Disodium at any time throughout the course of treatment for their cancer diagnosis, but in the event that the patient received the drug multiple times, only select the first time.
#' create_cohort(cohort = "NSCLC",
#'      regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
#'      regimen_order = 1,
#'      regimen_order_type = "within regimen")
#' Example4 ----------------------------------
#' # Create a cohort of all NSCLC patients who ever received Immunotherapy
#' create_cohort(cohort = "NSCLC",
#'      regimen_drugs = "Immunotherapy")
create_cohort <- function(cohort,
                          index_ca_seq = 1,
                          institution,
                          stage_dx,
                          ca_hist_adeno_squamous,
                          mets_dx_stage_iv,
                          regimen_drugs,
                          regimen_order,
                          regimen_order_type,
                          return_summary = FALSE) {

  # apply to all variables (alt would be r language)
  cohort_temp <- cohort

  # print the cohort that is being returned, esp useful for drug information
  # print(paste0(""))

  # check params
  if (length(cohort_temp) > 1) {
    stop("Specify only one cohort at a time.")
  }

  if (!(cohort %in% c("NSCLC", "CRC"))) {
    stop("Select from available cancer cohorts: NSCLC, CRC")
  }
#  if ( sum(!grepl("^NSCLC$", cohort)>0 , !missing(institution_temp) , !grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"), institution_temp)>0 ) >0  ){

#
#   if (cohort == "NSCLC" && !missing(institution) && !(institution %in% c("DFCI", "MSK", "VICC", "UHN"))){
#     stop("Select from available participating institutions. For NSCLC, the participating institutions were DFCI, MSK, UHN and VICC.")
#   }

  if(sum(!missing(institution),grepl("^NSCLC$", cohort)>0)>1 ){
    if( sum(!grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"), institution)>0)>0  ){
      stop("Select from available participating institutions. For NSCLC, the participating institutions were DFCI, MSK, UHN and VICC.")
    }
  }

  if(sum(!missing(institution),grepl("^CRC$", cohort)>0)>1 ){
    if(sum( !grepl(c("^DFCI$|^MSK$|^VICC$"), institution)>0 ) >0){
      stop("Select from available participating institutions. For CRC, the participating institutions were DFCI, MSK and VICC.")
    }
  }

  # if (cohort == "CRC" && !missing(institution) && !(institution %in% c("DFCI", "MSK", "VICC"))){
  #   stop("Select from available participating institutions. For CRC, the participating institutions were DFCI, MSK and VICC.")
  # }

  if (missing(institution) & cohort == "NSCLC"){
    institution_temp <- c("DFCI", "MSK", "UHN", "VICC")
    message("No institutions were provided defaults are DFCI, MSK, UHN, and VICC for NSCLC cohort")
  } else  if (missing(institution) & cohort == "CRC"){
    institution_temp <- c("DFCI", "MSK","VICC")
    message("No institutions were provided defaults are DFCI, MSK,and VICC for CRC cohort")
  } else {
    institution_temp <- {{ institution }}
  }
  # mets at diagnosis specified but stage 4 not selected

  # regimen_order_type needs to be specified if regimen_order is specified
  # this doesn't work
  if (missing(regimen_order_type) && !missing(regimen_order)) {
    stop("Regimen order type must be specified. Choose from 'within cancer' or 'within drug'.")
  }
#can't only specify regimen_order_type neec to build check for that

  if (missing(regimen_order_type)) {
    regimen_order_type <- NULL
  }

  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
      distinct(stage_dx), stage_dx)
  }
  else {
    stage_dx_temp <- {{ stage_dx }}
  }

  # to account for unspecified histology
  if (missing(ca_hist_adeno_squamous)) {
    histology_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
      distinct(ca_hist_adeno_squamous), ca_hist_adeno_squamous)
  }
  else {
    histology_temp <- {{ ca_hist_adeno_squamous }}
  }

  #################################################################################
  #                             pull cancer cohort                                #
  #################################################################################
  # select patients based on cohort, institution, stage at diagnosis, histology and cancer number
  cohort_ca_dx <- get(paste0("ca_dx_index_", cohort_temp)) %>%
    # renumber index cancer diagnoses
    group_by(cohort, record_id) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    # apply filter(s)
    filter(
      institution %in% c(institution_temp),
      stage_dx %in% c(stage_dx_temp),
      ca_hist_adeno_squamous %in% c(histology_temp),
      index_ca_seq %in% c({{ index_ca_seq }})
    )

  # pull drug regimens to those patients

  # option 1: all drug regimens to all patients in cohort
  # regimen_drugs is not specified, regimen_order is not specified
  # if (missing(regimen_drugs) & missing(regimen_order)) {
  cohort_ca_drugs <- left_join(cohort_ca_dx,
    get(paste0("ca_drugs_", cohort_temp)),
    by = c("cohort", "record_id", "institution", "ca_seq")
  ) %>%
    # create order for drug regimen within cancer and within times the drug was received
    group_by(cohort, record_id, ca_seq) %>%
    mutate(order_within_cancer = 1:n()) %>%
    ungroup() %>%
    group_by(cohort, record_id, ca_seq, regimen_drugs) %>%
    mutate(order_within_regimen = 1:n()) %>%
    ungroup() %>%
    left_join(.,
              regimen_drugs_lookup,
              by = c("regimen_drugs"))
  # }

  # option 2: all "first line" drug regimens (regimens of a certain number, within a cancer diganosis)
  # specific regimen number to all pts in cohort, any regimen name
  # regimen_drugs is not specified, regimen_order is specified and regimen_type = "within cancer"
  if (missing(regimen_drugs) && !missing(regimen_order) && stringr::str_to_lower(regimen_order_type) == "within cancer") {
    cohort_ca_drugs <- left_join(cohort_ca_dx,
      get(paste0("ca_drugs_", cohort_temp)),
      by = c("cohort", "record_id", "institution", "ca_seq")
    ) %>%
      filter(order_within_cancer %in% c({{ regimen_order }}))

    # restrict cancer cohort to all patients who got a drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        select(cohort, record_id, institution, ca_seq),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # if specific drug regimen is requested
  # option 3: all times that drug regimen was received
  if (!missing(regimen_drugs) && missing(regimen_order)) {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c({{ regimen_drugs }}) | abbreviation %in% c({{ regimen_drugs }}))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        select(cohort, record_id, institution, ca_seq),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 4: 1st (or other) time that regimen was received
  else if (!missing(regimen_drugs) && !missing(regimen_order) && stringr::str_to_lower(regimen_order_type) == "within regimen") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(regimen_drugs %in% c({{ regimen_drugs }}) | abbreviation %in% c({{ regimen_drugs }})) %>%
      # filter on order of interest (e.g. first, all)
      filter(order_within_regimen %in% c({{ regimen_order }}))


    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        select(cohort, record_id, institution, ca_seq),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # specific first line regimens
  else if (!missing(regimen_drugs) && !missing(regimen_order) && stringr::str_to_lower(regimen_order_type) == "within cancer") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      filter(
        regimen_drugs %in% c({{ regimen_drugs }} | abbreviation %in% c({{ regimen_drugs }})),
        order_within_cancer %in% c({{ regimen_order }})
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        select(cohort, record_id, institution, ca_seq),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # return a table 1 to describe the cancer cohort if the user specifies
  if (return_summary == TRUE) {
    tbl1_cohort <- gtsummary::tbl_summary(cohort_ca_dx,
      by = cohort,
      include = c(cohort, institution, stage_dx, ca_hist_adeno_squamous),
    )

    tbl_drugs <- gtsummary::tbl_summary(cohort_ca_drugs,
      by = cohort,
      include = c(cohort, institution, regimen_drugs)
    )
  }

  if (return_summary == TRUE) {
    return(list(
      "cohort_ca_dx" = cohort_ca_dx,
      "cohort_ca_drugs" = cohort_ca_drugs %>% select(-order_within_cancer, -order_within_regimen),
      "tbl1_cohort" = tbl1_cohort,
      "tbl_drugs" = tbl_drugs
    ))
    } else {
      return(list(
        "cohort_ca_dx" = cohort_ca_dx,
        "cohort_ca_drugs" = cohort_ca_drugs %>% select(-order_within_cancer, -order_within_regimen)

      ))
  }
}
