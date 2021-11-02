#' create_analytic_cohort
#'
#' This function allows the user to create a cohort from the GENIE BPC data
#' based on cancer diagnosis information such as cancer cohort, treating
#' institution, histology, and stage at diagnosis, as well as cancer-directed
#' regimen information including regimen name and regimen order.
#' This function returns two datasets:
#' (1) `cohort_ca_dx` will contain cancer diagnosis information for patients
#' matching the specified criteria. This dataset is structured as one record per
#'  patient per associated cancer diagnosis.
#' (2) `cohort_ca_drugs` will return the drug-regimen information for patients
#' matching the specified criteria. This dataset is structured as one record per
#'  patient, per regimen and associated cancer diagnosis. For example, a drug
#'  regimen associated with treating two cancer diagnoses will have two records
#'  in this dataset.
#' The function inputs `cohort`, `institution`, `stage_dx`,
#' and `regimen_drugs` correspond to the variable
#' names in the GENIE BPC Analytic Data Guide, available on
#' \href{https://www.synapse.org/#!Synapse:syn21241322}{Synapse}.
#'
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC"
#' (non-small cell lung cancer) or "CRC" (colorectal cancer). Future cohorts
#' will include "BrCa" (breast cancer), "PANC" (pancreatic cancer),
#' "Prostate" (prostate cancer).
#' @param data_synapse The list returned from pull_data_synapse().
#' @param index_ca_seq Index cancer sequence. Default is 1, indicating the
#' patient's first index cancer. The index cancer is also referred to as the
#' BPC Project cancer in the GENIE BPC Analytic Data Guide; this is the
#' cancer that met the eligibility criteria for the project and was selected
#' at random for PRISSMM phenomic data curation. Specifying multiple index
#' cancer sequences, e.g. index_ca_seq = c(1, 2) will return index cancers to
#' patients with 1 index cancer and will return the first AND second index
#' cancers to patients with multiple.
#' @param institution GENIE BPC participating institution. Must be one of
#' "DFCI", "MSK", "UHN", or "VICC" for NSCLC cohorts; must be one of "DFCI",
#' "MSK", "VICC" for CRC. Default selection is all institutions.
#' @param stage_dx Stage at diagnosis. Must be one of "Stage I", "Stage II",
#' "Stage III", "Stage I-III NOS", "Stage IV". Default selection is all stages.
#' @param histology Cancer histology. For all cancer cohorts except for BrCa
#' (breast cancer), this corresponds to the variable `ca_hist_adeno_squamous` and
#' must be one of
#' "Adenocarcinoma", "Squamous cell", "Sarcoma", "Small cell carcinoma", "Carcinoma",
#' "Other histologies/mixed tumor". For BrCa, this corresponds to the variable
#' `ca_hist_brca` and must be one of
#' "Invasive lobular carcinoma", "Invasive ductal carcinoma", "Other histology".
#' Default selection is all histologies.
#' @param regimen_drugs Vector with names of drugs in cancer-directed regimen,
#' separated by a comma. For example, to specify a regimen consisting of
#' Carboplatin and Pemetrexed, specify regimen_drugs = "Carboplatin,
#' Pemetrexed". Acceptable values are found in the `drug_regimen_list`
#' dataset provided with this package.
#' @param regimen_type Indicates whether the regimen(s) specified in
#' `regimen_drugs` indicates the exact regimen to return, or if regimens
#' containing the drugs listed in `regimen_drugs` should be returned. Must be
#' one of "Exact" or "Containing". The default is "Exact".
#' @param regimen_order Order of cancer-directed regimen. If multiple drugs
#' are specified, `regimen_order` indicates the regimen order for all drugs;
#' different values of `regimen_order` cannot be specified for different drug
#' regimens. If multiple values are specified, e.g. c(1, 2), then drug regimens
#' that met either order criteria are returned.
#' @param regimen_order_type Specifies whether the `regimen_order` parameter
#' refers to the order of receipt of the drug regimen within the cancer
#' diagnosis (across all other drug regimens; "within cancer") or the order of
#' receipt of the drug regimen within the times that that drug regimen was
#' administered (e.g. the first time carboplatin pemetrexed was received, out
#' of all times that the patient received carboplatin pemetrexed; "within
#' regimen"). Acceptable values are "within cancer" and "within regimen".
#' @param return_summary Specifies whether a summary table for the cohort is
#' returned. Default is FALSE. The `gtsummary` package is required to return a
#' summary table.
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
#' # create_analytic_cohort(cohort = "NSCLC",
#' #   stage_dx = c("Stage IV"),
#' #   histology = "Adenocarcinoma")
#'
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin,
#' # Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#' # for their first index NSCLC
#' # pull_data_synapse("NSCLC")
#' # create_analytic_cohort(cohort = "NSCLC",
#' #   regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin,
#' #   Etoposide"),
#' #   regimen_order = 1,
#' #   regimen_order_type = "within cancer")
#'
#' # Example 3 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed
#' # Disodium
#' # at any time throughout the course of treatment for their cancer diagnosis,
#' # but in the event that the patient received the drug multiple times,
#' # only select the first time.
#' # pull_data_synapse("NSCLC")
#' # create_analytic_cohort(cohort = "NSCLC",
#' #   regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
#' #   regimen_order = 1,
#' #   regimen_order_type = "within regimen")
#' @import
#' dplyr
#' purrr
#' stringr
create_analytic_cohort <- function(cohort,
                                   data_synapse,
                                   index_ca_seq = 1,
                                   institution,
                                   stage_dx,
                                   histology,
                                   regimen_drugs,
                                   regimen_type = "Exact",
                                   regimen_order,
                                   regimen_order_type,
                                   return_summary = FALSE) {

  # apply to all variables (alt would be r language)
  cohort_temp <- stringr::str_to_upper(cohort)

  # alphabetize drugs in regimen to match how they are stored in variable
  # regimen_drugs
  if (!missing(regimen_drugs)) {
    regimen_drugs_sorted <- map_chr(strsplit(regimen_drugs, ","), ~
    toString(str_to_lower(str_sort((str_trim(.x))))))
  }

  # check parameters
  # cancer cohort
  if (length(cohort_temp) > 1) {
    stop("Specify only one cohort at a time.")
  }

  if (!(stringr::str_to_upper(cohort) %in% c("NSCLC", "CRC", "BRCA"))) {
    stop("Select from available cancer cohorts: NSCLC, CRC, BrCa")
  }
  #  if ( sum(!grepl("^NSCLC$", cohort)>0 , !missing(institution_temp) ,
  # !grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"), institution_temp)>0 ) >0  ){

  # cohort object
  if (missing(data_synapse)) {
    stop("Specify the object created by pull_data_synapse() function.")
  }

  # index cancer sequence
  # get max # index cancers/pt
  max_index_ca <- pluck(data_synapse, paste0("ca_dx_index_", cohort_temp)) %>%
    group_by(cohort, record_id) %>%
    summarize(n_index = n(), .groups = "drop") %>%
    summarize(max_n_index = max(n_index))

  if (max(index_ca_seq) > max_index_ca) {
    stop(paste0(
      "There are no patients in the cohort with ", max_index_ca,
      " index cancer diagnoses. The maximum number of index cancers to
         one patient is ", max_index_ca, "."
    ))
  }
  # participating institutions by cohort
  if (sum(!missing(institution), grepl("^NSCLC$", stringr::str_to_upper(cohort)) > 0) > 1) {
    if (sum(!grepl(
      c("^DFCI$|^MSK$|^VICC$|^UHN$"),
      stringr::str_to_upper(institution)
    ) > 0) > 0) {
      stop("Select from available participating institutions. For NSCLC, the
           participating institutions were DFCI, MSK, UHN and VICC.")
    }
  }

  if (sum(!missing(institution), grepl("^CRC$|^BRCA$", stringr::str_to_upper(cohort)) > 0) > 1) {
    if (sum(!grepl(c("^DFCI$|^MSK$|^VICC$"), stringr::str_to_upper(institution))
    > 0) > 0) {
      stop("Select from available participating institutions. For CRC, the
           participating institutions were DFCI, MSK and VICC.")
    }
  }

  if (missing(institution) & stringr::str_to_upper(cohort) == "NSCLC") {
    institution_temp <- c("DFCI", "MSK", "UHN", "VICC")
  } else if (missing(institution) & stringr::str_to_upper(cohort) %in% c("CRC", "BRCA")) {
    institution_temp <- c("DFCI", "MSK", "VICC")
  } else {
    institution_temp <- stringr::str_to_upper({{ institution }})
  }

  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(pluck(data_synapse, paste0("ca_dx_index_", cohort_temp)) %>%
      dplyr::distinct(stage_dx), stage_dx)
  }
  else {
    stage_dx_temp <- {{ stage_dx }}
  }

  # stage mis-specified
  if (!missing(stage_dx) &&
    sum(!grepl(
      c("^stage i$|^stage ii$|^stage iii$|
                 ^stage i-iii nos$|^stage iv$"),
      stringr::str_to_lower(stage_dx)
    ) > 0) > 0) {
    stop("Select from available stages: Stage I, Stage II, Stage III,
         Stage I-III NOS, Stage IV")
  }

  # to account for unspecified histology
  if (missing(histology)) {
    if (cohort_temp != "BRCA") {
      histology_temp <- pull(pluck(data_synapse, paste0(
        "ca_dx_index_",
        cohort_temp
      )) %>%
        distinct(ca_hist_adeno_squamous), ca_hist_adeno_squamous)
    } else {
      histology_temp <- pull(
        pluck(data_synapse, paste0(
          "ca_dx_index_",
          cohort_temp
        )) %>%
          distinct(ca_hist_brca),
        ca_hist_brca
      )
    }
  }
  else {
    histology_temp <- {{ histology }}
  }

  # histology mis-specified
  if (!missing(histology) &&
    cohort_temp != "BRCA" &&
    sum(!grepl(
      c("^adenocarcinoma$|^squamous cell$|^sarcoma$|^small cell
                 carcinoma$|^carcinoma$|^other histologies/mixed tumor$"),
      stringr::str_to_lower(histology)
    ) > 0) > 0) {
    stop("Select from available histology categories: Adenocarcinoma,
         Squamous cell, Sarcoma, Small cell carcinoma, Other histologies/mixed
         tumor")
  }
  if (!missing(histology) &&
    cohort_temp == "BRCA" &&
    sum(!grepl(
      c("^invasive lobular carcinoma$|^invasive ductal carcinoma$|
                 ^Other histology$"),
      stringr::str_to_lower(histology)
    ) > 0) > 0) {
    stop("Select from available histology categories: Invasive lobular
         carcinoma, Invasive ductal carcinoma, Other histology")
  }

  ### drug regimen parameter checks
  # if regimen type is mis-specified
  if (!missing(regimen_type) | is.numeric(regimen_type)) {
    if (!(stringr::str_to_lower(regimen_type) %in% c("exact", "containing"))) {
      stop("For regimen_type select from 'exact' or 'containing'")
    }
  }

  # if regimen_order is not numeric
  if (!missing(regimen_order) && !is.numeric(regimen_order)) {
    stop("The regimen_order parameter must be a numeric value >=1.")
  }

  # if regimen_order_type is mis-specified
  if (!missing(regimen_order_type) &&
    (is.numeric(regimen_order_type) ||
      !(stringr::str_to_lower(regimen_order_type) %in% c(
        "within cancer",
        "within regimen"
      )))) {
    stop("For regimen_order_type select from 'within cancer' or
         'within regimen'")
  }

  # regimen_order_type needs to be specified if regimen_order is specified
  if (missing(regimen_order_type) && !missing(regimen_order)) {
    stop("Regimen order type must also be specified. Choose from
         'within cancer' or 'within regimen'")
  }

  # can't only specify regimen_order_type need to build check for that
  if (!missing(regimen_order_type) && missing(regimen_order)) {
    stop("Numeric order must also be specified in 'regimen_order' argument.")
  }

  # if regimen_type is specified, regimen_drugs must also be specified
  if (!missing(regimen_type) && missing(regimen_drugs)) {
    stop("If regimen_type is specified, regimen_drugs must also be specified.")
  }

  if (missing(regimen_order_type)) {
    regimen_order_type <- NULL
  }

  ##############################################################################
  #                             pull cancer cohort                             #
  ##############################################################################
  # select patients based on cohort, institution, stage at diagnosis,
  # histology and cancer number
  if (cohort_temp != "BRCA") {
    cohort_ca_dx <- pluck(data_synapse, paste0("ca_dx_index_", cohort_temp)) %>%
      # re-number index cancer diagnoses
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(index_ca_seq = 1:n()) %>%
      dplyr::ungroup() %>%
      # apply filter(s)
      dplyr::filter(
        stringr::str_to_lower(.data$institution) %in%
          stringr::str_to_lower(c(institution_temp)),
        stringr::str_to_lower(.data$stage_dx) %in%
          stringr::str_to_lower(c(stage_dx_temp)),
        stringr::str_to_lower(.data$ca_hist_adeno_squamous) %in%
          stringr::str_to_lower(c(histology_temp)),
        .data$index_ca_seq %in% c({{ index_ca_seq }})
      )
  } else {
    cohort_ca_dx <- pluck(data_synapse, paste0("ca_dx_index_", cohort_temp)) %>%
      # re-number index cancer diagnoses
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(index_ca_seq = 1:n()) %>%
      dplyr::ungroup() %>%
      # apply filter(s)
      dplyr::filter(
        stringr::str_to_lower(.data$institution) %in%
          stringr::str_to_lower(c(institution_temp)),
        stringr::str_to_lower(.data$stage_dx) %in%
          stringr::str_to_lower(c(stage_dx_temp)),
        stringr::str_to_lower(.data$ca_hist_brca) %in%
          stringr::str_to_lower(c(histology_temp)),
        .data$index_ca_seq %in% c({{ index_ca_seq }})
      )
  }

  # pull drug regimens to those patients


  # option 1: all drug regimens to all patients in cohort
  # regimen_drugs is not specified, regimen_order is not specified
  cohort_ca_drugs <- dplyr::inner_join(cohort_ca_dx,
    pluck(data_synapse, paste0("ca_drugs_", cohort_temp)),
    by = c("cohort", "record_id", "institution", "ca_seq")
  ) %>%
    # create order for drug regimen within cancer and within times the
    # drug was received
    dplyr::group_by(.data$cohort, .data$record_id, .data$ca_seq) %>%
    dplyr::arrange(.data$cohort, .data$record_id, .data$ca_seq, .data$regimen_number) %>%
    dplyr::mutate(order_within_cancer = 1:n()) %>%
    dplyr::ungroup() %>%
    # order drugs w/in regimen, have to account for structure of data which is
    # 1 reg:assoc ca dx
    # (may have more than one row for a drug regimen even if it's the first time
    # that drug regimen was received)
    dplyr::left_join(.,
      pluck(data_synapse, paste0("ca_drugs_", cohort_temp)) %>%
        dplyr::distinct(
          .data$record_id, .data$regimen_number,
          .data$regimen_drugs
        ) %>%
        dplyr::group_by(.data$record_id, .data$regimen_drugs) %>%
        dplyr::arrange(
          .data$record_id, .data$regimen_number,
          .data$regimen_drugs
        ) %>%
        dplyr::mutate(order_within_regimen = 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$regimen_drugs),
      by = c("record_id", "regimen_number")
    ) %>%
    dplyr::left_join(.,
      regimen_abbreviations,
      by = c("regimen_drugs")
    )

  # option 2: all "first line" drug regimens (regimens of a certain number,
  # within a cancer diganosis)
  # specific regimen number to all pts in cohort, any regimen name
  # regimen_drugs is not specified, regimen_order is specified and
  # regimen_type = "within cancer"
  if (missing(regimen_drugs) && !missing(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {

    # cohort_ca_drugs <- dplyr::left_join(cohort_ca_dx,
    #   pluck(data_synapse, paste0("ca_drugs_", cohort_temp)),
    #   by = c("cohort", "record_id", "institution", "ca_seq")
    # ) %>%
    #   dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }}))

    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }}))

    # restrict cancer cohort to all patients who got a drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }}))%>%
        dplyr::select(
          .data$cohort,
          .data$record_id,
          .data$institution,
          .data$ca_seq
        ),
      by = c(
        "cohort", "record_id", "institution", "ca_seq"
      )
    )

  }

  # if specific drug regimen is requested; exact regimen
  # option 3a: all times that exact drug regimen was received
  if (!missing(regimen_drugs) && missing(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "exact") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(
        str_to_lower(.data$regimen_drugs) %in% c(regimen_drugs_sorted) |
          str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted) #|
        # drug_class %in% c(regimen_drugs_sorted)
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        dplyr::distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 3b: all times that regimen containing drugs was received
  if (!missing(regimen_drugs) && missing(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "containing") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(grepl(
        paste(regimen_drugs_sorted, collapse = "|"),
        str_to_lower(.data$regimen_drugs)
      ) |
        grepl(
          paste(regimen_drugs_sorted, collapse = "|"),
          str_to_lower(.data$abbreviation)
        ))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        dplyr::distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 4a: 1st (or other) time that exact regimen was received
  if (!missing(regimen_drugs) && !missing(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within regimen" &&
    stringr::str_to_lower(regimen_type) == "exact") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(str_to_lower(.data$regimen_drugs) %in% c(regimen_drugs_sorted) |
        str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted)) %>%
      # filter on order of interest (e.g. first, all)
      dplyr::filter(.data$order_within_regimen %in% c({{ regimen_order }}))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 4b: 1st (or other) time that regimen containing was received
  if (!missing(regimen_drugs) &&
    !missing(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within regimen" &&
    stringr::str_to_lower(regimen_type) == "containing") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(grepl(
        paste(regimen_drugs_sorted, collapse = "|"),
        str_to_lower(.data$regimen_drugs)
      ) |
        grepl(
          paste(regimen_drugs_sorted, collapse = "|"),
          str_to_lower(.data$abbreviation)
        )) %>%
      # need to re-create order of interest because it was defined based on the
      # exact regimen
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(order_within_containing_regimen = 1:n()) %>%
      dplyr::ungroup() %>%
      # filter on order of interest (e.g. first, all)
      dplyr::filter(.data$order_within_containing_regimen
        %in% c({{ regimen_order }}))

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        dplyr::distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 5a: specific drugs within a cancer diagnosis, exact regimen
  if (!missing(regimen_drugs) &&
    !missing(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "exact" &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(
        str_to_lower(.data$regimen_drugs) %in% c(regimen_drugs_sorted) |
          str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted),
        .data$order_within_cancer %in% c({{ regimen_order }})
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # option 5b: specific drugs within a cancer diagnosis, regimen containing
  if (!missing(regimen_drugs) &&
    !missing(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "containing" &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- cohort_ca_drugs %>%
      dplyr::filter(
        grepl(paste(regimen_drugs_sorted, collapse = "|"), str_to_lower(.data$regimen_drugs)) |
          grepl(
            paste(regimen_drugs_sorted, collapse = "|"),
            str_to_lower(.data$abbreviation)
          ),
        .data$order_within_cancer %in% c({{ regimen_order }})
      )

    # restrict cancer cohort to patients on that drug regimen
    cohort_ca_dx <- dplyr::inner_join(cohort_ca_dx,
      cohort_ca_drugs %>%
        dplyr::distinct(
          .data$cohort, .data$record_id, .data$institution,
          .data$ca_seq
        ),
      by = c("cohort", "record_id", "institution", "ca_seq")
    )
  }

  # for patients meeting the specified criteria, also pull cancer panel
  # test information
  cohort_ngs <- fetch_samples(
    cohort = cohort_temp,
    data_synapse = data_synapse,
    df_record_ids = cohort_ca_dx
  )

  # if 0 patients are returned
  if (nrow(cohort_ca_dx) == 0) {
    message("No patients meeting the specified criteria were returned.
            Ensure that all parameters were correctly specified. Specifically,
            the list of acceptable drugs can be found in the
            `drug_regimen_list` dataset available with this package.")
  }

  # return a table 1 to describe the cancer cohort if the user specifies
  if (nrow(cohort_ca_dx) > 0 && return_summary == TRUE) {

    # number of records per patient in the diagnosis dataset
    n_rec_dx_dset <- cohort_ca_dx %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = .data$n_rec_pt,
        label = n_rec_pt ~ "Number of diagnoses per patient on cohort_ca_dx",
        type = n_rec_pt ~ "categorical"
      ) %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} patients**"
        ),
        quiet = TRUE
      )

    n_rec_drugs_dset <- cohort_ca_drugs %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = .data$n_rec_pt,
        label = n_rec_pt ~ "Number of regimens per patient in cohort_ca_drugs",
        type = n_rec_pt ~ "categorical"
      )

    n_rec_cpt_dset <- cohort_ngs %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = .data$n_rec_pt,
        label = n_rec_pt ~ "Number of CPTs per patient in cohort_ngs",
        type = n_rec_pt ~ "categorical"
      )

    tbl_overall_summary <- gtsummary::tbl_stack(
      tbls = list(
        n_rec_dx_dset,
        n_rec_drugs_dset,
        n_rec_cpt_dset
      ),
      quiet = TRUE
    )

    if (cohort_temp != "BRCA") {
      tbl_cohort <- cohort_ca_dx %>%
        # dplyr::group_by(.data$record_id) %>%
        # dplyr::mutate(n_rec_pt = n()) %>%
        # dplyr::ungroup() %>%
        gtsummary::tbl_summary(
          include = c(
            .data$cohort, .data$institution,
            .data$stage_dx, .data$ca_hist_adeno_squamous
          )
        ) %>%
        gtsummary::modify_header(
          update = list(
            stat_0 ~ "**N = {N} Diagnoses**"
          ),
          quiet = TRUE
        )
    } else {
      tbl_cohort <- cohort_ca_dx %>%
        # dplyr::group_by(.data$record_id) %>%
        # dplyr::mutate(n_rec_pt = n()) %>%
        # dplyr::ungroup() %>%
        gtsummary::tbl_summary(
          include = c(
            .data$cohort, .data$institution,
            .data$stage_dx, .data$ca_hist_brca
          )
        ) %>%
        gtsummary::modify_header(
          update = list(
            stat_0 ~ "**N = {N} Diagnoses**"
          ),
          quiet = TRUE
        )
    }

    tbl_drugs <- cohort_ca_drugs %>%
      # dplyr::group_by(.data$cohort, .data$institution,
      #                 .data$record_id, .data$ca_seq) %>%
      # dplyr::mutate(n_rec_pt = n()) %>%
      # dplyr::ungroup() %>%
      gtsummary::tbl_summary(
        include = c(
          .data$cohort, .data$institution,
          .data$regimen_drugs
        )
      ) %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} Regimens**"
        ),
        quiet = TRUE
      )

    tbl_ngs <- cohort_ngs %>%
      gtsummary::tbl_summary(
        include = c(
          .data$cohort, .data$institution, .data$cpt_oncotree_code,
          .data$cpt_seq_assay_id
        )
      ) %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} Cancer Panel Tests**"
        ),
        quiet = TRUE
      )
  }

  if (nrow(cohort_ca_dx) > 0 && return_summary == TRUE) {
    return(list(
      "cohort_ca_dx" = cohort_ca_dx %>% select(-.data$index_ca_seq),
      "cohort_ca_drugs" = cohort_ca_drugs #%>%
        # dplyr::select(
        #   -.data$order_within_cancer, -.data$order_within_regimen,
        #   -.data$index_ca_seq, -.data$abbreviation
        # )
      ,
      "cohort_ngs" = cohort_ngs,
      "tbl_overall_summary" = tbl_overall_summary,
      "tbl_cohort" = tbl_cohort,
      "tbl_drugs" = tbl_drugs,
      "tbl_ngs" = tbl_ngs
    ))
  } else if (nrow(cohort_ca_dx) > 0) {
    return(list(
      "cohort_ca_dx" = cohort_ca_dx %>% select(-.data$index_ca_seq),
      "cohort_ca_drugs" = cohort_ca_drugs %>%
        dplyr::select(
          -.data$order_within_cancer, -.data$order_within_regimen,
          -.data$index_ca_seq, -.data$abbreviation
        ),
      "cohort_ngs" = cohort_ngs
    ))
  }
} # end of function
