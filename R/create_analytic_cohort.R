#' Select cohort of patients for analysis
#'
#' This function allows the user to create a cohort from the GENIE BPC data
#' based on cancer diagnosis information such as cancer cohort, treating
#' institution, histology, and stage at diagnosis, as well as cancer-directed
#' regimen information including regimen name and regimen order. This function
#' returns each of the clinical and genomic data files subset on the patients
#' that met criteria for the analytic cohort. Documentation regarding the
#' structure and contents of each file can be found in the Analytic Data Guide
#' corresponding to each data release, as well as in the
#' \href{https://genie-bpc.github.io/genieBPC/articles/clinical_data_structure_vignette.html}{Clinical Data Structure vignette}.
#'
#' See the \href{https://genie-bpc.github.io/genieBPC/articles/create_analytic_cohort_vignette.html}{create_analytic_cohort vignette} for further documentation and examples.
#'
#' @param data_synapse The item from the nested list returned from
#' pull_data_synapse() that corresponds to the cancer cohort of interest.
#' @param index_ca_seq Index cancer sequence. Default is 1, indicating the
#' patient's first index cancer. The index cancer is also referred to as the
#' BPC Project cancer in the GENIE BPC Analytic Data Guide; this is the
#' cancer that met the eligibility criteria for the project and was
#' selected at random for PRISSMM phenomic data curation.
#' Specifying multiple index cancer sequences, e.g.
#' index_ca_seq = c(1, 2) will return index cancers to
#' patients with 1 index cancer and will return the first AND second index
#' cancers to patients with multiple.
#' @param institution GENIE BPC participating institution. See lookup table
#'  `cohort_institution` for a list of institutions participating in curation of
#'  each cancer cohort. Default selection is all institutions. This parameter
#'  corresponds to the variable `institution` in the Analytic Data Guide.
#' @param stage_dx Stage at diagnosis. Must be one of "Stage I", "Stage II",
#' "Stage III", "Stage I-III NOS", "Stage IV". The default selection is all
#' stages.
#' Note that if this parameter is specified, any cases that are missing stage
#' information are automatically excluded from the resulting cohort.
#' This parameter corresponds to the variable `stage_dx` in the
#' Analytic Data Guide.
#' @param histology Cancer histology. For all cancer cohorts except for BrCa
#' (breast cancer), this parameter corresponds to the variable
#' `ca_hist_adeno_squamous` and must be one of "Adenocarcinoma",
#' "Squamous cell", "Sarcoma",
#' "Small cell carcinoma", "Carcinoma", "Other histologies/mixed tumor".
#'  For BrCa, this parameter corresponds to the variable
#' `ca_hist_brca` and must be one of
#' "Invasive lobular carcinoma", "Invasive ductal carcinoma", "Other histology".
#' The default selection is all histologies. Note that if this parameter is
#' specified, any cases that are missing histology information are automatically
#' excluded from the resulting cohort.
#' @param regimen_drugs Vector with names of drugs in cancer-directed regimen,
#' separated by a comma. For example, to specify a regimen consisting of
#' Carboplatin and Pemetrexed, specify regimen_drugs = "Carboplatin,
#' Pemetrexed". Acceptable values are found in the `drug_regimen_list`
#' dataset provided with this package. This parameter
#' corresponds to the variable `regimen_drugs` in the Analytic Data Guide.
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
#' @return A list of data frames containing clinical and next generation
#' sequencing information for patients that met the specified criteria.
#' Optionally, if return_summary = TRUE, the list also includes summary
#' tables for the number of records per dataset (`tbl_overall_summary`)
#' as well as tables of key cancer diagnosis (`tbl_cohort`),
#' cancer-directed regimen (`tbl_drugs`) and next generation sequencing
#' (`tbl_ngs`) variables.
#'
#' @author Jessica Lavery
#' @export
#'
#' @examples
#' # Examples using package test data
#' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC adenocarcinoma and
#' # obtain all of their corresponding clinical and genomic data
#'
#' ex1 <- create_analytic_cohort(
#'   data_synapse = genieBPC::nsclc_test_data,
#'   stage_dx = "Stage IV",
#'   histology = "Adenocarcinoma"
#' )
#'
#' names(ex1)
#'
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin,
#' # Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#' # for their first index NSCLC
#'
#' ex2 <- create_analytic_cohort(
#'   data_synapse = genieBPC::nsclc_test_data,
#'   regimen_drugs = c(
#'     "Cisplatin, Pemetrexed Disodium",
#'     "Cisplatin, Etoposide"
#'   ),
#'   regimen_order = 1,
#'   regimen_order_type = "within cancer"
#' )
#'
#' # Example 3 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed
#' # Disodium at any time throughout the course of treatment for their
#' # cancer diagnosis,
#' # but in the event that the patient received the drug multiple times,
#' # only select the first time.
#'
#' ex3 <- create_analytic_cohort(
#'   data_synapse = genieBPC::nsclc_test_data,
#'   regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
#'   regimen_order = 1,
#'   regimen_order_type = "within regimen"
#' )
#'
#' @examplesIf genieBPC::.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))
#' # Example 4 ----------------------------------
#' # Using create_analytic_cohort with pull_data_synapse
#' set_synapse_credentials()
#'
#' nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#'
#' ex4 <- create_analytic_cohort(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
#'   regimen_order = 1,
#'   regimen_order_type = "within regimen"
#' )
#'
#' @import
#' dplyr
#' purrr
#' stringr
create_analytic_cohort <- function(data_synapse,
                                   index_ca_seq = 1,
                                   institution,
                                   stage_dx,
                                   histology,
                                   regimen_drugs,
                                   regimen_type = "Exact",
                                   regimen_order,
                                   regimen_order_type,
                                   return_summary = FALSE) {


# check input parameters --------------------------------------------------
  # cohort object
  if (missing(data_synapse)) {
    stop("Specify the cohort object from the nested list returned by the
         pull_data_synapse() function.")
  } else if (is.null(data_synapse)) {
    stop("The object specified for data_synapse does not exist.")
  }

  # determine if the user passed a single cancer cohort object
  # or an object with multiple cohorts pulled
  # if data_synapse_depth = 4, they supplied the pull_data_synapse() object
  # if data_synapse_depth = 3, they supplied the data for a single cohort
  # release, nested within the pull_data_synapse() object
  # e.g., pull_data_syn_obj$NSCLC_v2.0 (depth 3) vs pull_data_syn_obj (depth 4)
  data_synapse_depth <- purrr::pluck_depth(data_synapse)

  # if multiple cohorts are supplied, stack the data together before proceeding
  # 1. convert to data frame (required if one cohort as TM or Rad Tx data and
  # another does not, need to convert blank list element to dataframe in order
  # to proceed with the mutate)
  # 2. Transpose the list to combine each dataset for all cohorts
  # 3. convert any variables with different types (numeric vs character)
  # across data releases to be character in order to stack data together
  # 4. stack corresponding datasets together
  if (data_synapse_depth == 4){
    # check input parameter
    # trying to check that the pull_data_synapse object returned is
    # specific to the cohort
    purrr::map(data_synapse, function(x) {
      names <- names(x)

      if (!("pt_char" %in% names)) {
        stop(c(
          "The data_synapse parameter is expecting a nested list of ",
          "GENIE BPC datasets as returned from `pull_data_synapse()`."
        ))
      }
    })

    # keep only most recent data releases
    releases_supplied <- tidyr::tibble(cohort_release =
                                  stringr::str_remove_all(pattern = "-pharma|-consortium|-public",
                                                 string = names(data_synapse)))


    # only keep the most recent release for each cohort
    releases_supplied_clean <- left_join(releases_supplied,
                                         synapse_version() %>%
                                           tidyr::separate(version,
                                                           into = c("version", "access"),
                                                           sep = "-") %>%
                                           dplyr::mutate(cohort_release = paste0(.data$cohort, "_", .data$version)) %>%
                                           dplyr::select("cohort_release", "release_date"),
                                         by = c("cohort_release")) %>%
      tidyr::separate(.data$cohort_release, into = c("cohort", "release"), sep = "_",
                      remove = FALSE) %>%
      dplyr::group_by(.data$cohort) %>%
      dplyr::slice_max(.data$release_date) %>%
      dplyr::ungroup()

    # check for multiple releases to a single cohort
    if (length(setdiff(releases_supplied$cohort_release, releases_supplied_clean$cohort_release)) > 0) {
      cli::cli_inform("Note: Multiple data releases were supplied for the same cancer cohort. Only data corresponding to the most recent data release for each cohort are returned.")
    }

    # only keep most recent releases before continuing to process
    data_synapse <- data_synapse %>%
      purrr::keep(str_remove_all(pattern = "-pharma|-consortium|-public",
                                 string = names(.))
                  %in% c(releases_supplied_clean$cohort_release))

    # print message if not all datasets are available for all cohorts
    dset_datarelease_check <- purrr::map(data_synapse, names) %>%
      purrr::map_df(., ~tibble(dset = .), .id = "cohort_release") %>%
      dplyr::group_by(.data$dset) %>%
      dplyr::summarize(cohorts = paste0(.data$cohort_release, collapse = ", "),
                       n_cohort_release = n()) %>%
      # only keep datasets not available for all releases
      dplyr::filter(.data$n_cohort_release != length(data_synapse))

    if (nrow(dset_datarelease_check) > 0) {
      cli::cli_inform(paste0("Note: Some datasets are only available for select data releases, including ",
                             paste0(dset_datarelease_check$dset, " (only available for ", dset_datarelease_check$cohorts, ")", collapse = ", ")))
    }

    # transpose the list and keep only the most recent data release
    data_synapse <- purrr::list_transpose(data_synapse) %>%
      purrr::map_depth(., .depth = 2, as.data.frame) %>%
      purrr::map_depth(., .depth = 2, ~mutate(.x,
                                       across(any_of(c("release_version",
                                                       "naaccr_laterality_cd",
                                                       "naaccr_tnm_path_desc",
                                                       "pdl1_iclrange",
                                                       "pdl1_iclrange_2",
                                                       "pdl1_icurange",
                                                       "pdl1_icurange_2",
                                                       "pdl1_tcurange",
                                                       "pdl1_lcpsrange",
                                                       "pdl1_ucpsrange",
                                                       "cpt_seq_date",
                                                       "Match_Norm_Seq_Allele1",
                                                       "Match_Norm_Seq_Allele2",
                                                       "Protein_position")), ~as.character(.)))) %>%
      # this only applies when multiple cohorts are supplied
      purrr::map(., bind_rows, .id = "cohort_release") %>%
      purrr::map(., mutate, cohort_release = str_remove_all(.data$cohort_release,
                                                            pattern = "-public|-consortium|-pharma"))

  }

  # get cohort name and how it is capitalized in the data_synapse object
  cohort_temp <- pull(
    purrr::pluck(data_synapse, "pt_char") %>%
      # remove digits to account for Phase 2 Cohorts
      dplyr::mutate(cohort_no_digits = stringr::str_remove_all(pattern = "[:digit:]",
                              string = .data$cohort)) %>%
      dplyr::distinct(.data$cohort_no_digits),
    "cohort_no_digits"
  )

  # alphabetize drugs in regimen to match
  # how they are stored in variable
  # regimen_drugs
  if (!missing(regimen_drugs)) {
    regimen_drugs_sorted <- purrr::map_chr(
      strsplit(regimen_drugs, ","), ~
        toString(stringr::str_to_lower(str_sort(
          (stringr::str_trim(.x))
        )))
    )
  }

  # index cancer sequence
  # get max # index cancers/pt
  max_index_ca <- pluck(data_synapse, "ca_dx_index") %>%
    group_by(.data$cohort, .data$record_id) %>%
    summarize(n_index = n(), .groups = "drop") %>%
    summarize(max_n_index = max(.data$n_index))

  if (max(index_ca_seq) > max_index_ca) {
    stop(paste0(
      "There are no patients in the cohort with >", max_index_ca,
      " index cancer diagnoses. The maximum number of index cancers to
         one patient is ", max_index_ca, "."
    ))
  }

  # check participating institutions by cohort
  # if no specified cohorts have the institution available
  if (!missing(institution)){
    # collect specified cohort and institution
    ca_inst_specified <- tibble(
      cohort = cohort_temp,
      institution = stringr::str_to_upper(institution)
    )

    # if any institutions specified that are not available for a cohort
    chk_inst <- setdiff(ca_inst_specified, genieBPC::cohort_institution) %>%
      mutate(cohort_institution = paste0(.data$cohort, "-", .data$institution))

    if (nrow(chk_inst) == nrow(ca_inst_specified)){
      stop("The specified institution is not available for any of the cancer cohorts specified. Review the lookup table `cohort_institution` for the list of available institutions by cohort.")
    } else if (0 < nrow(chk_inst) & nrow(chk_inst) < nrow(ca_inst_specified)) {
      # if some of the cohorts don't have the specified institution available
      cli::cli_inform((paste0("The specified institution did not contribute data for all cancer cohorts provided (",
                     chk_inst$cohort_institution, ")")))

      institution_temp <- stringr::str_to_upper({{ institution }})
    } else {
    institution_temp <- stringr::str_to_upper({{ institution }})
  }
  } else if (missing(institution)){
    institution_temp <- genieBPC::cohort_institution %>%
      dplyr::filter(.data$cohort %in% c(cohort_temp)) %>%
      dplyr::pull(institution)
  }

  # to account for unspecified stage
  if (missing(stage_dx)) {
    stage_dx_temp <- pull(pluck(data_synapse, "ca_dx_index") %>%
      dplyr::distinct(stage_dx), stage_dx)
  } else {
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
    if (max(grepl("BrCa", cohort_temp)) == 0) {
      histology_temp <- pull(pluck(data_synapse, "ca_dx_index") %>%
                               distinct(.data$ca_hist_adeno_squamous), .data$ca_hist_adeno_squamous)
    } else {
      # available histologies
      avail_histology <- data_synapse$ca_dx_index %>%
        mutate(histology_aggregated = case_when(
          .data$cohort == "BrCa" ~ ca_hist_brca,
          TRUE ~ ca_hist_adeno_squamous
        )) %>%
        distinct(.data$histology_aggregated)

      histology_temp <- pull(avail_histology, "histology_aggregated")
    }
  } else {
    histology_temp <- {{ histology }}
  }

  # histology mis-specified
  if (!missing(histology) &&
    sum(!grepl(
      c("^adenocarcinoma$|^squamous cell$|^sarcoma$|^small cell carcinoma$|^carcinoma$|^other histologies/mixed tumor$|^invasive lobular carcinoma$|^invasive ductal carcinoma$|^other histology$"),
      histology, ignore.case = TRUE) > 0) > 0) {
    stop("Select from available histology categories: Adenocarcinoma,
         Squamous cell, Sarcoma, Small cell carcinoma, Other histologies/mixed
         tumor (NSCLC, CRC, PANC, Prostate, BLADDER) or Invasive lobular
         carcinoma, Invasive ductal carcinoma, Other histology (BrCa).")
  }


# drug regimen parameter checks -------------------------------------------
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

  # can't only specify regimen_order_type
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


# pull cancer cohort ------------------------------------------------------
  # select patients based on cohort, institution, stage at diagnosis,
  # histology and cancer number
  if (!purrr::is_empty(grep("BrCa", cohort_temp))) {
    cohort_ca_dx <- pluck(data_synapse, "ca_dx_index") %>%
      # re-number index cancer diagnoses
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(index_ca_seq = 1:n(),
                    histology_aggregated = case_when(
                      cohort == "BrCa" ~ ca_hist_brca,
                      TRUE ~ ca_hist_adeno_squamous
                    )) %>%
      dplyr::ungroup() %>%
      # apply filter(s)
      dplyr::filter(
        stringr::str_to_lower(.data$institution) %in%
          stringr::str_to_lower(c(institution_temp)),
        stringr::str_to_lower(.data$stage_dx) %in%
          stringr::str_to_lower(c(stage_dx_temp)),
        stringr::str_to_lower(.data$histology_aggregated) %in%
          stringr::str_to_lower(c(histology_temp)),
        .data$index_ca_seq %in% c({{ index_ca_seq }})
      ) %>%
      dplyr::select(-"histology_aggregated")
  } else {
    cohort_ca_dx <- pluck(data_synapse, "ca_dx_index") %>%
      # re-number index cancer diagnoses
      dplyr::group_by(.data$cohort, .data$record_id) %>%
      dplyr::mutate(index_ca_seq = 1:n()) %>%
      dplyr::ungroup() %>%
      # # apply filter(s)
      dplyr::filter(
        stringr::str_to_lower(.data$institution) %in%
          stringr::str_to_lower(c(institution_temp)),
        stringr::str_to_lower(.data$stage_dx) %in%
          stringr::str_to_lower(c(stage_dx_temp)),
        stringr::str_to_lower(.data$ca_hist_adeno_squamous) %in%
          stringr::str_to_lower(c(histology_temp)),
        .data$index_ca_seq %in% c({{ index_ca_seq }})
      )
  }




# pull drug regimens to those patients ------------------------------------
  # option 1: all drug regimens to all patients in cohort
  # regimen_drugs is not specified, regimen_order is not specified
  cohort_ca_drugs <- dplyr::inner_join(cohort_ca_dx %>%
                                         dplyr::select("cohort", "record_id", "ca_seq"),
                                       pluck(data_synapse, "ca_drugs"),
                                       by = c("cohort", "record_id", "ca_seq")
  ) %>%
    # create order for drug regimen within cancer and within times the
    # drug was received
    dplyr::group_by(.data$cohort, .data$record_id, .data$ca_seq) %>%
    dplyr::arrange(
      .data$cohort, .data$record_id,
      .data$ca_seq, .data$regimen_number
    ) %>%
    dplyr::mutate(order_within_cancer = 1:n()) %>%
    dplyr::ungroup() %>%
    # order drugs w/in regimen, have to account for structure of data which is
    # 1 reg:assoc ca dx
    # (may have more than one row for a drug regimen even if it's the first time
    # that drug regimen was received)
    dplyr::left_join(.,
      pluck(data_synapse, "ca_drugs") %>%
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
        dplyr::select(-"regimen_drugs"),
      by = c("record_id", "regimen_number")
    ) %>%
    dplyr::left_join(.,
      genieBPC::regimen_abbreviations,
      by = c("regimen_drugs")
    )



  # option 2: all "first line" drug regimens (regimens of a certain number,
  # within a cancer diagnosis)
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
                                        dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }})) %>%
                                        dplyr::select("cohort", "record_id", "institution", "ca_seq"),
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
      dplyr::filter(str_to_lower(.data$regimen_drugs)
      %in% c(regimen_drugs_sorted) |
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
    # have to start with full drugs dataset for 'within regimen',
    # otherwise are left with all drug regimens to pts in this cohort
    cohort_ca_drugs <- pluck(data_synapse, "ca_drugs") %>%
      # add on abbreviations
      dplyr::left_join(.,
        genieBPC::regimen_abbreviations,
        by = c("regimen_drugs")
      ) %>%
      # create new order b/c this is regimen CONTAINING drugs listed
      # order drugs w/in regimen, have to account for
      # structure of data which is
      # 1 reg:assoc ca dx
      # (may have more than one row for a drug regimen even
      # if it's the first time
      # that drug regimen was received)
      # have to filter on containing regimens first, then re-number
      dplyr::filter(grepl(
        paste(regimen_drugs_sorted, collapse = "|"),
        str_to_lower(.data$regimen_drugs)
      ) |
        grepl(
          paste(regimen_drugs_sorted, collapse = "|"),
          str_to_lower(.data$abbreviation)
        )) %>%
      # now re-number w/in containing regimens
      dplyr::left_join(.,
        pluck(data_synapse, "ca_drugs") %>%
          # add on abbreviations
          dplyr::left_join(.,
            genieBPC::regimen_abbreviations,
            by = c("regimen_drugs")
          ) %>%
          # get regimens containing drugs of interest
          dplyr::filter(grepl(
            paste(regimen_drugs_sorted, collapse = "|"),
            str_to_lower(.data$regimen_drugs)
          ) |
            grepl(
              paste(regimen_drugs_sorted, collapse = "|"),
              str_to_lower(.data$abbreviation)
            )) %>%
          # get distinct regimen administrations (since regs
          # potentially mapped to multiple ca types)
          dplyr::distinct(
            .data$record_id, .data$regimen_number,
            .data$regimen_drugs
          ) %>%
          # order regimens
          dplyr::group_by(.data$record_id) %>%
          dplyr::arrange(
            .data$record_id, .data$regimen_number,
            .data$regimen_drugs
          ) %>%
          dplyr::mutate(
            order_within_containing_regimen = 1:n()
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-"regimen_drugs"),
        by = c("record_id", "regimen_number")
      ) %>%
      # filter on order of interest (e.g. first, all)
      dplyr::filter(.data$order_within_containing_regimen
                    %in% c({{ regimen_order }})) %>%
      # restrict to patients in the cohort (started with all regimens to all
      # patients)
      dplyr::inner_join(.,
                        cohort_ca_dx %>%
                          dplyr::select("cohort", "record_id", "ca_seq"),
                        by = c("cohort", "record_id", "ca_seq")
      ) %>%
      # create blank variables (dropped below, not having them is unique to
      # regimen_order_type = 'containing')
      mutate(
        order_within_cancer = as.numeric(NA),
        order_within_regimen = as.numeric(NA)
      )

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
        grepl(paste(regimen_drugs_sorted,
          collapse = "|"
        ), str_to_lower(.data$regimen_drugs)) |
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


# pull other datasets for patients meeting incl criteria ------------------
  # patient characteristics
  cohort_pt_char <- dplyr::inner_join(cohort_ca_dx %>%
                                        dplyr::select("cohort", "record_id"),
                                      pluck(data_synapse, "pt_char"),
                                      by = c("cohort", "record_id")
  )

  # non-index cancer
  cohort_ca_dx_non_index <- dplyr::inner_join(cohort_ca_dx %>%
                                                dplyr::select("cohort", "record_id"),
                                              pluck(data_synapse, "ca_dx_non_index"),
                                              by = c("cohort", "record_id")
  )

  # PRISSMM Path
  cohort_prissmm_pathology <- dplyr::inner_join(cohort_ca_dx %>%
                                                  dplyr::select("cohort", "record_id"),
                                                pluck(data_synapse, "prissmm_pathology"),
                                                by = c("cohort", "record_id")
  )

  # PRISSMM Imaging
  cohort_prissmm_imaging <- dplyr::inner_join(cohort_ca_dx %>%
                                                dplyr::select("cohort", "record_id"),
                                              pluck(data_synapse, "prissmm_imaging"),
                                              by = c("cohort", "record_id")
  )

  # PRISSMM Med Onc
  cohort_prissmm_md <- dplyr::inner_join(cohort_ca_dx %>%
                                           dplyr::select("cohort", "record_id"),
                                         pluck(data_synapse, "prissmm_md"),
                                         by = c("cohort", "record_id")
  )

  # TM (if applicable)
  if (!is.null(pluck(data_synapse, "tumor_marker"))) {
    cohort_tumor_marker <- dplyr::inner_join(cohort_ca_dx %>%
                                               dplyr::select("cohort", "record_id"),
                                             pluck(data_synapse, "tumor_marker"),
                                             by = c("cohort", "record_id")
    )
  }

  # RT (if applicable)
  if (!is.null(pluck(data_synapse, "ca_radtx"))) {
    cohort_ca_radtx <- dplyr::inner_join(cohort_ca_dx %>%
                                           dplyr::select("cohort", "record_id", "ca_seq"),
                                         pluck(data_synapse, "ca_radtx"),
                                         by = c("cohort", "record_id", "ca_seq")
    )
  }

  # cancer panel test information
  # keep records based on record_id + cancer sequence of interest
  cohort_ngs <- dplyr::inner_join(
    cohort_ca_dx %>%
      dplyr::select("cohort", "record_id", "ca_seq"),
    pluck(data_synapse, "cpt"),
    by = c("cohort", "record_id", "ca_seq")
  ) %>%
    distinct()

  if(any(names(cohort_ngs) == "cpt_sample_type") &
     !any(names(cohort_ngs) == "sample_type")){

    cohort_ngs <- cohort_ngs %>%
      dplyr::mutate(sample_type = case_when(
        str_to_lower(.data$cpt_sample_type)
        %in% c("1", "primary", "primary tumor") ~ "Primary tumor",
        str_to_lower(.data$cpt_sample_type)
        %in% c("2", "lymph node metastasis") ~ "Lymph node metastasis",
        str_to_lower(.data$cpt_sample_type)
        %in% c("3", "distant organ metastasis") ~ "Distant organ metastasis",
        str_to_lower(.data$cpt_sample_type)
        %in% c("4", "metastasis site unspecified", "metastatic recurrence") ~
          "Metastasis site unspecified",
        str_to_lower(.data$cpt_sample_type)
        %in% c("5", "local recurrence") ~ "Local recurrence",
        str_to_lower(.data$cpt_sample_type)
        %in% c("6", "unspecified") ~ as.character(NA),
        str_to_lower(.data$cpt_sample_type)
        %in% c("7", "not applicable or hematologic malignancy") ~
          "Not applicable or hematologic malignancy"
      ))
  }

  # genomic sequencing information
  if (!is.null(pluck(data_synapse, "fusions"))) {
    cohort_fusions <- dplyr::inner_join(pluck(data_synapse, "fusions"),
                                        cohort_ngs %>%
                                          dplyr::select("cohort", "cpt_genie_sample_id"),
                                        by = c("Tumor_Sample_Barcode" = "cpt_genie_sample_id")
    )
  }

  if (!is.null(pluck(data_synapse, "sv"))) {
    cohort_sv <- dplyr::inner_join(pluck(data_synapse, "sv"),
                                   cohort_ngs %>%
                                     dplyr::select("cohort", "cpt_genie_sample_id"),
                                   by = c("Sample_Id" = "cpt_genie_sample_id")
    )
  }

  if (!is.null(pluck(data_synapse, "mutations_extended"))) {
    cohort_mutations_extended <- dplyr::inner_join(pluck(data_synapse,
                                                         "mutations_extended"),
                                                   cohort_ngs %>%
                                                     dplyr::select("cohort", "cpt_genie_sample_id"),
                                                   by = c("Tumor_Sample_Barcode" = "cpt_genie_sample_id")
    )
  }

  # cna file is 1 col / tumor sample barcode
  if (!is.null(pluck(data_synapse, "cna"))) {
    # get list of IDs to keep
    cpt_barcode_keep <- pluck(data_synapse, "cpt") %>%
      mutate(
        Tumor_Sample_Barcode =
          stringr::str_replace_all(.data$cpt_genie_sample_id,
            pattern = "-",
            replacement = "\\."
          )
      ) %>%
      pull("Tumor_Sample_Barcode")

    cohort_cna <- pluck(data_synapse, "cna") %>%
      select("Hugo_Symbol", any_of(c(cpt_barcode_keep, "cohort_release")))
  }

  # if 0 patients are returned for all cancer cohorts
  if (nrow(cohort_ca_dx) == 0) {
    cli::cli_inform(("No patients meeting the specified criteria were returned.
            Ensure that all parameters were correctly specified. Review the applicable institution, histology, and regimen drugs parameters to ensure values relevant for all cohorts are supplied. Specifically, the list of acceptable drugs can be found in the `drug_regimen_list` dataset and the list of relevant institutions can be found in the `cohort_institution` dataset available with this package."))
  } else if (length(setdiff(cohort_temp, cohort_ca_dx$cohort)) > 0){
    # no patients met criteria for some cancer cohorts
    cli::cli_inform((paste0("Note: No patients meeting the specified criteria were returned for some cancer cohorts supplied (" ,
                   paste0(setdiff(cohort_temp, cohort_ca_dx$cohort),
                          collapse = ", "),
                   "). \n Review the applicable institution, histology, and regimen drugs parameters to ensure values relevant for all cohorts are supplied. Specifically, the list of acceptable drugs can be found in the `drug_regimen_list` dataset and the list of relevant institutions can be found in the `cohort_institution` dataset available with this package."))
    )
  }



# return tables -----------------------------------------------------------
  # return a table 1 to describe the cancer cohort if the user specifies
  if (nrow(cohort_ca_dx) > 0 && return_summary == TRUE) {

    # number of records per patient in the diagnosis dataset
    n_rec_dx_dset <- cohort_ca_dx %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = "n_rec_pt",
        label = n_rec_pt ~ "Number of diagnoses per patient in cohort_ca_dx
        data frame",
        type = n_rec_pt ~ "categorical"
      ) %>%
      gtsummary::modify_header(stat_0 ~ "**N = {N} patients**")

    n_rec_drugs_dset <- cohort_ca_drugs %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = "n_rec_pt",
        label = n_rec_pt ~ "Number of regimens per patient in cohort_ca_drugs
        data frame",
        type = n_rec_pt ~ "categorical"
      )

    n_rec_cpt_dset <- cohort_ngs %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = "n_rec_pt",
        label = n_rec_pt ~ "Number of CPTs per patient in cohort_ngs
        data frame",
        type = n_rec_pt ~ "categorical"
      )

    tbl_overall_summary <- gtsummary::tbl_stack(
      tbls = list(
        n_rec_dx_dset,
        n_rec_drugs_dset,
        n_rec_cpt_dset
      ),
      quiet = TRUE
    ) %>%
      gtsummary::bold_labels()

    if (purrr::is_empty(grep("BrCa", cohort_temp))) {
      tbl_cohort <- cohort_ca_dx %>%
        gtsummary::tbl_summary(
          include = c(
            "cohort", "institution",
            "stage_dx", "ca_hist_adeno_squamous"
          ),
          label = list(
            cohort ~ "Cohort (cohort)",
            institution ~ "Institution (institution)",
            stage_dx ~ "Stage at diagnosis (stage_dx)",
            ca_hist_adeno_squamous ~ "Histology (ca_hist_adeno_squamous)"
          )
        ) %>%
        gtsummary::bold_labels() %>%
        gtsummary::modify_header(stat_0 ~ "**N = {N} Diagnoses**")
    } else {
      tbl_cohort <- cohort_ca_dx %>%
        # dplyr::group_by(.data$record_id) %>%
        # dplyr::mutate(n_rec_pt = n()) %>%
        # dplyr::ungroup() %>%
        gtsummary::tbl_summary(
          include = c("cohort", "institution", "stage_dx",
                             "ca_hist_brca"),
          label = list(
            cohort ~ "Cohort (cohort)",
            institution ~ "Institution (institution)",
            stage_dx ~ "Stage at diagnosis (stage_dx)",
            ca_hist_brca ~ "Histology (ca_hist_brca)"
          )
        ) %>%
        gtsummary::bold_labels() %>%
        gtsummary::modify_header(stat_0 ~ "**N = {N} Diagnoses**")
    }

    tbl_drugs <- cohort_ca_drugs %>%
      gtsummary::tbl_summary(
        include = c("cohort", "institution", "regimen_drugs"),
        label = list(
          cohort ~ "Cohort (cohort)",
          institution ~ "Institution (institution)",
          regimen_drugs ~ "Drugs in regimen (regimen_drugs)"
        )
      ) %>%
      gtsummary::bold_labels() %>%
      gtsummary::modify_header(stat_0 ~ "**N = {N} Regimens**")

    tbl_ngs <- cohort_ngs %>%
      gtsummary::tbl_summary(
        include = c("cohort", "institution",
                           "cpt_oncotree_code", "cpt_seq_assay_id"),
        label = list(
          cohort ~ "Cohort (cohort)",
          institution ~ "Institution (institution)",
          cpt_oncotree_code ~ "OncoTree code (cpt_oncotree_code)",
          cpt_seq_assay_id ~ "Sequence assay ID (cpt_seq_assay_id)"
        )
      ) %>%
      gtsummary::bold_labels() %>%
      gtsummary::modify_header(stat_0 ~ "**N = {N} Cancer Panel Tests**")
  }

  # drop variable before returning data frame
  cohort_ca_dx <- cohort_ca_dx %>% select(-"index_ca_seq")

  cohort_ca_drugs <- cohort_ca_drugs %>%
    dplyr::select(-"order_within_cancer",
                  -"order_within_regimen",
                  -"abbreviation")

  # order of dataframes, should they exist
  df_order <- c(
    "cohort_pt_char", "cohort_ca_dx",
    "cohort_ca_dx_non_index",
    "cohort_ca_radtx", "cohort_ca_drugs",
    "cohort_prissmm_imaging", "cohort_prissmm_pathology",
    "cohort_prissmm_md", "cohort_tumor_marker",
    "cohort_ngs",
    "cohort_mutations_extended", "cohort_fusions", "cohort_sv", "cohort_cna",
    "tbl_overall_summary", "tbl_cohort", "tbl_drugs", "tbl_ngs"
  )

  # return data frames & tables that are present in the function's environment
  rtn <- mget(ls(environment(), pattern = "^cohort_|^tbl"),
    envir = environment()
  )



  # save elements on list in order that we want (clinical datasets, genomic
  # datasets, tables) and drop any items that don't appear in this run of
  # create_analytic_cohort
  rtn_ordered <- rtn[c(df_order)] %>%
    purrr::compact()


  if (nrow(cohort_ca_dx) > 0) {
    return(rtn_ordered)
  }
} # end of function
