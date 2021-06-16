#' create_cohort
#'
#' This function allows the user to create a cohort from the GENIE BPC data based on cancer diagnosis information such as cancer cohort, treating institution, histology, and stage at diagnosis, as well as cancer-directed regimen information including regimen name and regimen order.
#' This function returns two datasets:
#' (1) `cohort_ca_dx` will contain cancer diagnosis information for patients matching the specified criteria. This dataset is structured as one record per patient per associated cancer diagnosis.
#' (2) `cohort_ca_drugs` will return the drug-regimen information for patients matching the specified criteria. This dataset is structured as one record per patient, per regimen and associated cancer diagnosis. For example, a drug regimen associated with treating two cancer diagnoses will have two records in this dataset.
#' The function inputs `cohort`, `institution`, `stage_dx`, `ca_hist_adeno_squamous`, and `regimen_drugs` correspond to the variable names in the GENIE BPC Analytic Data Guide, available on  \href{https://www.synapse.org/#!Synapse:syn21241322}{Synapse}.
#'
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC" (non-small cell lung cancer) or "CRC" (colorectal cancer). Future cohorts will include "BrCa" (breast cancer), "PANC" (pancreatic cancer), "Prostate" (prostate cancer).
#' @param index_ca_seq Index cancer sequence. Default is 1, indicating the patient's first index cancer. The index cancer is also referred to as the BPC Project cancer in the GENIE BPC Analytic Data Guide; this is the cancer that met the eligibility criteria for the project and was selected at random for PRISSMM phenomic data curation.
#' @param institution GENIE BPC participating institution. Must be one of "DFCI", "MSK", "UHN", or "VICC" for NSCLC cohorts; must be one of "DFCI", "MSK", "VICC" for CRC. Default selection is all institutions.
#' @param stage_dx Stage at diagnosis. Must be one of "Stage I", "Stage II", "Stage III", "Stage I-III NOS", "Stage IV". Default selection is all stages.
#' @param ca_hist_adeno_squamous Cancer histology. Must be one of "Adenocarcinoma", "Squamous cell", "Sarcoma", "Small cell carcinoma", "Other histologies/mixed tumor". Default selection is all histologies.
#' @param regimen_drugs Vector with names of drugs in cancer-directed regimen, separated by a comma. For example, to specify a regimen consisting of Carboplatin and Pemetrexed, specify regimen_drugs = "Carboplatin, Pemetrexed". Acceptable values are found in the `drug_names_by_cohort` dataset provided with this package.
#' @param regimen_type Indicates whether the regimen(s) specified in `regimen_drugs` indicates the exact regimen to return, or if regimens containing the drugs listed in `regimen_drugs` should be returned. Must be one of "Exact" or "Containing". The default is "Exact".
#' @param regimen_order Order of cancer-directed regimen. If multiple drugs are specified, `regimen_order` indicates the regimen order for all drugs; different values of `regimen_order` cannot be specified for different drug regimens.
#' @param regimen_order_type Specifies whether the `regimen_order` parameter refers to the order of receipt of the drug regimen within the cancer diagnosis (across all other drug regimens; "within cancer") or the order of receipt of the drug regimen within the times that that drug regimen was administered (e.g. the first time carboplatin pemetrexed was received, out of all times that the patient received carboplatin pemetrexed; "within regimen"). Acceptable values are "within cancer" and "within regimen".
#' @param return_summary Specifies whether a summary table for the cohort is returned. Default is FALSE. The `gtsummary` package is required to return a summary table.
#'
#' @return Returns data frames `cohort_ca_dx` and `cohort_ca_drugs`
#'
#' @author Jessica Lavery
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC adenocarcinoma and
#' # also return all of their corresponding cancer-directed drugs
#' # pull_data_synapse("NSCLC")
#' # create_cohort(cohort = "NSCLC",
#' #   stage_dx = c("Stage IV"),
#' #   ca_hist_adeno_squamous = "Adenocarcinoma")
#'
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin,
#' # Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#' # for their first index NSCLC
#' # pull_data_synapse("NSCLC")
#' # create_cohort(cohort = "NSCLC",
#' #   regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin, Etoposide"),
#' #   regimen_order = 1,
#' #   regimen_order_type = "within cancer")
#'
#' # Example 3 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed Disodium
#' # at any time throughout the course of treatment for their cancer diagnosis,
#' # but in the event that the patient received the drug multiple times,
#' # only select the first time.
#' # pull_data_synapse("NSCLC")
#' # create_cohort(cohort = "NSCLC",
#' #   regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
#' #   regimen_order = 1,
#' #   regimen_order_type = "within regimen")
#' @import
#' dplyr
#' purrr
create_cohort <- function(cohort,
                          index_ca_seq = 1,
                          institution,
                          stage_dx,
                          ca_hist_adeno_squamous,
                          regimen_drugs,
                          regimen_type = "Exact",
                          regimen_order,
                          regimen_order_type,
                          return_summary = FALSE) {

  # apply to all variables (alt would be r language)
  cohort_temp <- cohort

  # alphabetize drugs in regimen to match how they are stored in variable regimen_drugs
  if (!missing(regimen_drugs)) {
    regimen_drugs_sorted <- map_chr(strsplit(regimen_drugs, ","), ~toString(str_sort((str_trim(.x)))))
  }

  # check parameters
  # cancer cohort
  if (length(cohort_temp) > 1) {
    stop("Specify only one cohort at a time.")
  }

  if (!(cohort %in% c("NSCLC", "CRC"))) {
    stop("Select from available cancer cohorts: NSCLC, CRC")
  }
  #  if ( sum(!grepl("^NSCLC$", cohort)>0 , !missing(institution_temp) , !grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"), institution_temp)>0 ) >0  ){

  # participating institutions by cohort
  if(sum(!missing(institution), grepl("^NSCLC$", cohort)>0)>1 ){
    if( sum(!grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"),  stringr::str_to_upper(institution))>0)>0){
      stop("Select from available participating institutions. For NSCLC, the participating institutions were DFCI, MSK, UHN and VICC.")
    }
  }

  if(sum(!missing(institution),grepl("^CRC$", cohort)>0)>1 ){
    if(sum( !grepl(c("^DFCI$|^MSK$|^VICC$"), stringr::str_to_upper(institution))>0)>0){
      stop("Select from available participating institutions. For CRC, the participating institutions were DFCI, MSK and VICC.")
    }
  }

  if (missing(institution) & cohort == "NSCLC"){
    institution_temp <- c("DFCI", "MSK", "UHN", "VICC")
  } else  if (missing(institution) & cohort == "CRC"){
    institution_temp <- c("DFCI", "MSK","VICC")
  } else {
    institution_temp <- {{ institution }}
  }

  # mets at diagnosis specified but stage 4 not selected
  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
                            dplyr::distinct(stage_dx), stage_dx)
  }
  else {
    stage_dx_temp <- {{ stage_dx }}
  }

  # stage mis-specified
  if (!missing(stage_dx) &&
      sum(!grepl(c("^stage i$|^Stage ii$|^stage iii$|^stage i-iii nos$|^stage iv$"), stringr::str_to_lower(stage_dx))>0)>0) {
    stop("Select from available stages: Stage I, Stage II, Stage III, Stage I-III NOS, Stage IV")
  }

  # to account for unspecified histology
  if (missing(ca_hist_adeno_squamous)) {
    histology_temp <- pull(get(paste0("ca_dx_index_", cohort_temp)) %>%
                             distinct(ca_hist_adeno_squamous), ca_hist_adeno_squamous)
  }
  else {
    histology_temp <- {{ ca_hist_adeno_squamous }}
  }

  # histology mis-specified
  if (!missing(ca_hist_adeno_squamous) &&
      sum(!grepl(c("^adenocarcinoma$|^squamous cell$|^sarcoma$|^small cell carcinoma$|^other histologies/mixed tumor$"), stringr::str_to_lower(ca_hist_adeno_squamous)) >0)>0) {
    stop("Select from available histology categories: Adenocarcinoma, Squamous cell, Sarcoma, Small cell carcinoma, Other histologies/mixed tumor")
  }

  ### drug regimen parameter checks
  # if regimen type is mis-specified
  if (!missing(regimen_type) | is.numeric(regimen_type)) {
    if (!(stringr::str_to_lower(regimen_type) %in% c("exact", "containing"))){
      stop("For regimen_type select from 'exact' or 'containing'")
    }
  }

  # if regimen_order is not numeric
  if (!missing(regimen_order) && !is.numeric(regimen_order)) {
    stop("The regimen_order parameter must be a numeric value >=1.")
  }

  # if regimen_order_type is mis-specified
  if(!missing(regimen_order_type) &&
      (is.numeric(regimen_order_type) ||
       !(stringr::str_to_lower(regimen_order_type) %in% c("within cancer", "within regimen")))) {
    stop("For regimen_order_type select from 'within cancer' or 'within regimen'")
  }

  # regimen_order_type needs to be specified if regimen_order is specified
  if (missing(regimen_order_type) && !missing(regimen_order)) {
    stop("Regimen order type must also be specified. Choose from 'within cancer' or 'within regimen'")
  }

  # can't only specify regimen_order_type need to build check for that
  if(!missing(regimen_order_type) && missing(regimen_order) ){
    stop("Numeric order must also be specified in 'regimen_order' argument.")
  }

  # if regimen_type is specified, regimen_drugs must also be specified
  if (!missing(regimen_type) && missing(regimen_drugs)){
    stop("If regimen_type is specified, regimen_drugs must also be specified.")
  }

  if (missing(regimen_order_type)) {
    regimen_order_type <- NULL
  }

  #################################################################################
  #                             pull cancer cohort                                #
  #################################################################################
  # select patients based on cohort, institution, stage at diagnosis, histology and cancer number
  cohort_ca_dx <- get(paste0("ca_dx_index_", cohort_temp)) %>%
    # renumber index cancer diagnoses
    dplyr::group_by(.data$cohort, .data$record_id) %>%
    dplyr::mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    # apply filter(s)
    filter(
      stringr::str_to_lower(.data$institution) %in% stringr::str_to_lower(c(institution_temp)),
      stringr::str_to_lower(.data$stage_dx) %in% stringr::str_to_lower(c(stage_dx_temp)),
      stringr::str_to_lower(.data$ca_hist_adeno_squamous) %in% stringr::str_to_lower(c(histology_temp)),
      index_ca_seq %in% c({{ .env$index_ca_seq }})
    )

  # pull drug regimens to those patients

  # option 1: all drug regimens to all patients in cohort
  # regimen_drugs is not specified, regimen_order is not specified
  cohort_ca_drugs <- dplyr::left_join(cohort_ca_dx,
                               get(paste0("ca_drugs_", cohort_temp)),
                               by = c("cohort", "record_id", "institution", "ca_seq")
  ) %>%
    # create order for drug regimen within cancer and within times the drug was received
    dplyr::group_by(cohort, record_id, ca_seq) %>%
    dplyr::mutate(order_within_cancer = 1:n()) %>%
    dplyr::ungroup() %>%
    # order drugs w/in regimen, have to account for structure of data which is 1 reg:assoc ca dx
    # (may have more than one row for a drug regimen even if it's the first time that drug regimen was received)
    dplyr::left_join(.,
              get(paste0("ca_drugs_", cohort_temp)) %>%
                distinct(.data$record_id, .data$regimen_number, .data$regimen_drugs) %>%
                group_by(.data$record_id, .data$regimen_drugs) %>%
                arrange(.data$record_id, .data$regimen_number, .data$regimen_drugs) %>%
                mutate(order_within_regimen = 1:n()) %>%
                ungroup() %>%
                select(-.data$regimen_drugs),
              by = c("record_id", "regimen_number")) %>%
    dplyr::left_join(.,
              regimen_abbreviations,
              by = c("regimen_drugs")) 

  # option 2: all "first line" drug regimens (regimens of a certain number, within a cancer diganosis)
  # specific regimen number to all pts in cohort, any regimen name
  # regimen_drugs is not specified, regimen_order is specified and regimen_type = "within cancer"
  if (missing(regimen_drugs) && !missing(regimen_order) &&
      stringr::str_to_lower(regimen_order_type) == "within cancer") {
    cohort_ca_drugs <- dplyr::left_join(cohort_ca_dx,
                                 get(paste0("ca_drugs_", cohort_temp)),
                                 by = c("cohort", "record_id", "institution", "ca_seq")
    ) %>%
      filter(order_within_cancer %in% c({{ regimen_order }}))

    # restrict cancer cohort to all patients who got a drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 dplyr::select(.data$cohort, .data$record_id, .data$institution, .data$ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # if specific drug regimen is requested; exact regimen
  # option 3a: all times that exact drug regimen was received
  if (!missing(regimen_drugs) && missing(regimen_order) && regimen_type == "Exact") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(.data$regimen_drugs %in% c(regimen_drugs_sorted) |
               .data$abbreviation %in% c(regimen_drugs_sorted) #|
               # drug_class %in% c(regimen_drugs_sorted)
             )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 dplyr::distinct(.data$cohort, .data$record_id, .data$institution, .data$ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 3b: all times that regimen containing drugs was received
  if (!missing(regimen_drugs) && missing(regimen_order) && regimen_type == "Containing") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(grepl(paste(regimen_drugs_sorted, collapse = "|"), regimen_drugs) |
               grepl(paste(regimen_drugs_sorted, collapse = "|"), abbreviation))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 distinct(cohort, record_id, institution, ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 4a: 1st (or other) time that exact regimen was received
  if (!missing(regimen_drugs) && !missing(regimen_order) &&
      stringr::str_to_lower(regimen_order_type) == "within regimen" &&
      regimen_type == "Exact") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(regimen_drugs %in% c(regimen_drugs_sorted) | abbreviation %in% c(regimen_drugs_sorted)) %>%
      # filter on order of interest (e.g. first, all)
      dplyr::filter(order_within_regimen %in% c({{ regimen_order }}))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 distinct(.data$cohort, .data$record_id, .data$institution, .data$ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 4b: 1st (or other) time that regimen containing was received
  if (!missing(regimen_drugs) &&
      !missing(regimen_order) &&
      stringr::str_to_lower(regimen_order_type) == "within regimen" &&
      regimen_type == "Containing") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(grepl(paste(regimen_drugs_sorted, collapse = "|"), regimen_drugs) |
               grepl(paste(regimen_drugs_sorted, collapse = "|"), abbreviation)) %>%
      # need to re-create order of interest because it was defined based on the exact regimen
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(order_within_containing_regimen = 1:n()) %>%
      dplyr::ungroup() %>%
      # filter on order of interest (e.g. first, all)
      filter(order_within_containing_regimen %in% c({{ regimen_order }}))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 distinct(cohort, record_id, institution, ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 5a: specific drugs within a cancer diagnosis, exact regimen
  if (!missing(regimen_drugs) &&
      !missing(regimen_order) &&
      regimen_type == "Exact" &&
      stringr::str_to_lower(regimen_order_type) == "within cancer") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(
        .data$regimen_drugs %in% c(regimen_drugs_sorted) | abbreviation %in% c(regimen_drugs_sorted),
        .data$order_within_cancer %in% c({{ regimen_order }})
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 distinct(.data$cohort, .data$record_id, .data$institution, .data$ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 5b: specific drugs within a cancer diagnosis, regimen containing
  if (!missing(regimen_drugs) &&
      !missing(regimen_order) &&
      regimen_type == "Containing" &&
      stringr::str_to_lower(regimen_order_type) == "within cancer") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(grepl(paste(regimen_drugs_sorted, collapse = "|"), regimen_drugs) |
               grepl(paste(regimen_drugs_sorted, collapse = "|"), abbreviation),
             .data$order_within_cancer %in% c({{ regimen_order }})
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
                               cohort_ca_drugs %>%
                                 dplyr::distinct(.data$cohort, .data$record_id, .data$institution, .data$ca_seq),
                               by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # if 0 patients are returned
  if (nrow(cohort_ca_dx) == 0) {
    message("No patients meeting the specified criteria were returned. Ensure that all parameters were correctly specified. Specifically, the list of acceptable drugs can be found in the `drug_names_by_cohort` dataset available with this package.")
  }

  # return a table 1 to describe the cancer cohort if the user specifies
  if (nrow(cohort_ca_dx) > 0 & return_summary == TRUE) {
    tbl1_cohort <- cohort_ca_dx %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::mutate(n_rec_pt = n()) %>%
      dplyr::ungroup() %>%
      gtsummary::tbl_summary(by = cohort,
                             include = c(cohort, n_rec_pt, institution, stage_dx, ca_hist_adeno_squamous),
                             label = n_rec_pt ~ "Number of records per patient",
                             type = n_rec_pt ~ "categorical"
      )

    tbl_drugs <- cohort_ca_drugs %>%
      dplyr::group_by(record_id) %>%
      dplyr::mutate(n_rec_pt = n()) %>%
      dplyr::ungroup() %>%
      gtsummary::tbl_summary(by = cohort,
                             include = c(cohort, n_rec_pt, institution, regimen_drugs),
                             label = n_rec_pt ~ "Number of records per patient",
                             type = n_rec_pt ~ "categorical"
      )
  }

  if (nrow(cohort_ca_dx) > 0 & return_summary == TRUE) {
    return(list(
      "cohort_ca_dx" = cohort_ca_dx,
      "cohort_ca_drugs" = cohort_ca_drugs %>% dplyr::select(-order_within_cancer, -order_within_regimen),
      "tbl1_cohort" = tbl1_cohort,
      "tbl_drugs" = tbl_drugs
    ))
  } else if (nrow(cohort_ca_dx) > 0) {
    return(list(
      "cohort_ca_dx" = cohort_ca_dx,
      "cohort_ca_drugs" = cohort_ca_drugs %>% dplyr::select(-order_within_cancer, -order_within_regimen)
    ))
  }


  } # end of function
