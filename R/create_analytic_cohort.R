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
#' @param data_synapse To run create_analytic_cohort() for a single data
#'   release, the item from the nested list returned from `pull_data_synapse()`
#'   that corresponds to the cancer cohort of interest. To run
#'   create_analytic_cohort() for multiple cancer cohorts/data releases, supply
#'   the list returned from the `pull_data_synapse()` function after pulling
#'   multiple data releases.
#' @param index_ca_seq Index cancer sequence. Default is 1, indicating the
#' patient's first index cancer. The index cancer is also referred to as the
#' BPC Project cancer in the GENIE BPC Analytic Data Guide; this is the
#' cancer that met the eligibility criteria for the project and was
#' selected at random for PRISSMM phenomic data curation.
#' Specifying multiple index cancer sequences, e.g.
#' index_ca_seq = c(1, 2) will return index cancers to
#' patients with 1 index cancer and will return the first AND second index
#' cancers to patients with multiple.
#' @param institution GENIE BPC participating institution. Must be one of
#' "DFCI", "MSK", "UHN", or "VICC" for NSCLC, BLADDER, Prostate, and PANC cohorts; must be one of "DFCI",
#' "MSK", "VICC" for CRC and BrCa. Default selection is all institutions.
#' This parameter corresponds to the variable `institution` in the
#' Analytic Data Guide.
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
#' @author Jessica Lavery, Hannah Fuchs
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
                                   institution = NULL,
                                   stage_dx = NULL,
                                   histology = NULL,
                                   regimen_drugs = NULL,
                                   regimen_type = "Exact",
                                   regimen_order = NULL,
                                   regimen_order_type = NULL,
                                   return_summary = FALSE) {

  # check parameters
  # cohort object
  if (missing(data_synapse)) {
    stop("Specify the object returned from the call to the pull_data_synapse() function.")
  }

  # determine if the user passed a single cancer cohort object
  # or an object with multiple cohorts pulled
  # if data_synapse_depth = 4, they supplied the pull_data_synapse() object
  # if data_synapse_depth = 3, they supplied the data for a single cohort
  # release, nested within the pull_data_synapse() object
  # e.g., pull_data_syn_obj$NSCLC_v2.0 (depth 3) vs pull_data_syn_obj (depth 4)
  data_synapse_depth <- purrr::pluck_depth(data_synapse)

  # if the data_synapse list specified is of depth 3 (i.e., user specified
  # pull_data_syn_obj$data_release), then make into higher level list to use
  # purrr mapping below

  # browser()

  # the purpose of this is to maintain backwards compatibility with the user
  # specifying the data release object from pull_data_synapse, as in the
  # original release of this package, while utilizing the updated code in
  # create_analytic_cohort() that now processes multiple cohorts at a time
  if (data_synapse_depth == 3){
    # name of data_synapse input object
    data_synapse_character <- deparse(substitute(data_synapse))

    # if data_synapse$data_release_vX.X is supplied (i.e., not mapping over
    # create_analytic_cohort)
    if (grepl("\\$", data_synapse_character)){
      # save original data_synapse object
      data_synapse_original <- data_synapse

      # update data_synapse object to be a nested list
      assign("data_synapse", list(data_synapse_original))

      names(data_synapse) <- word(data_synapse_character, 2, sep = "\\$")
      # else conditions refer to when create_analytic_cohort is used in a map
      # if release version is present (i.e., not NSCLC v1.1-consortium release)
    } else if (dplyr::select(purrr::pluck(data_synapse, "pt_char"),
                      any_of("release_version")) %>% ncol() == 1) {
      # NSCLC v1.1-consortium doesn't have a release variable
      coh <- purrr::pluck(data_synapse, "pt_char") %>%
        dplyr::mutate(coh_temp = str_remove_all(pattern = "[:digit:]",
                                     string = .data$cohort)) %>%
        dplyr::pull(.data$coh_temp) %>%
        unique()

      rel <- purrr::pluck(data_synapse, "pt_char") %>%
        dplyr::select(release_version) %>%
        tidyr::separate(release_version, into = c("v", "release"), sep = "-",
                        extra = "drop") %>%
        unique() %>%
        dplyr::pull(v)

      data_synapse_original <- data_synapse

      data_synapse <- list(data_synapse_original)

      names(data_synapse) <- paste0(coh, "_v", rel)

    } else if (dplyr::select(pluck(data_synapse, "pt_char"),
                      any_of("release_version")) %>% ncol() == 0) {
    #   # NSCLC v1.1-consortium is the only data release doesn't have a
    #   # release variable
      data_synapse_original <- data_synapse

      data_synapse <- list(data_synapse_original)

      names(data_synapse) <- "NSCLC_v1.1"
    }
  }

  # browser()

  # check input parameter
  # check that the pull_data_synapse object returned is specific to the cohort
  purrr::map(data_synapse, function(x) {
    names <- names(x)

    if (!("pt_char" %in% names)) {
      stop(c(
        "The data_synapse parameter is expecting a nested list of ",
        "GENIE BPC datasets as returned from `pull_data_synapse()`."
      ))
    }
  })


  # if (!(stringr::str_to_upper(cohort) %in% c("NSCLC", "CRC", "BRCA"))) {
  #   stop("Select from available cancer cohorts:
  #        NSCLC, CRC, BrCa (not case sensitive)")
  # }

  #  if ( sum(!grepl("^NSCLC$", cohort)>0 , !missing(institution_temp) ,
  # !grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"), institution_temp)>0 ) >0  ){

  # get cohort name
  cohort_temp <- purrr::map(data_synapse, ~pluck(.x, "pt_char") %>%
                              # remove digits to account for Phase 2 Cohorts
                              mutate(cohort_no_digits = stringr::str_remove_all(
                                pattern = "[:digit:]",
                                string = .data$cohort)) %>%
                              distinct(.data$cohort_no_digits) %>%
                              pull("cohort_no_digits") %>%
                              unique())

  # alphabetize drugs in regimen to match
  # how they are stored in variable
  # regimen_drugs
  if (!missing(regimen_drugs)) {
    regimen_drugs_sorted <- purrr::map_chr(
      strsplit(regimen_drugs, ","), ~
        toString(str_to_lower(str_sort(
          (str_trim(.x))
        )))
    )
  }

  # index cancer sequence
  # get max # index cancers/pt
  max_index_ca <- purrr::map(data_synapse, ~ .x %>%
      pluck("ca_dx_index") %>%
      group_by(.data$cohort, .data$record_id) %>%
      summarize(n_index = n(), .groups = "drop") %>%
      summarize(max_n_index = max(.data$n_index)))

  purrr::map(max_index_ca, function(x) {
    if (max(index_ca_seq) > x$max_n_index) {
      stop(paste0(
        "There are no patients in the cohort with ", x$max_n_index,
        " index cancer diagnoses. The maximum number of index cancers for
         one patient is ", x$max_n_index, "."
      ))
    }
  })

  # institutions
  # first check all provided are included for that cohort
  # then assign if not missing
  # if missing, assign all for a given cohort
  institution_temp <- if (!is.null(institution)) {
    purrr::map(1:length(data_synapse), function(x) {
      if (startsWith(stringr::str_to_upper(cohort_temp[[x]]), "NSCLC")) {
        if (!all(grepl(c("^DFCI$|^MSK$|^VICC$|^UHN$"),
                       stringr::str_to_upper(institution)))) {
          stop(
            "Select from available participating institutions. For NSCLC, the
           participating institutions were DFCI, MSK, UHN and VICC."
          )
        } else {
          stringr::str_to_upper({{ institution }})
        }
      } else if (stringr::str_to_upper(cohort_temp[[x]]) %in% c("CRC", "BRCA")) {
        if (length(cohort_temp) == 1 & !all(grepl(
          c("^DFCI$|^MSK$|^VICC$"),
          stringr::str_to_upper(institution)
        ))) {
          stop(
            "Select from available participating institutions. For CRC and BrCa,
            the participating institutions were DFCI, MSK and VICC."
          )
        } else {
          institution[grepl(c("^DFCI$|^MSK$|^VICC$"), str_to_upper(institution))]
        }

        # if passes through errors, will do the following
      }
    })
  } else {
    purrr::map(1:length(data_synapse), function(x) {
      if (stringr::str_to_upper(cohort_temp[[x]]) %in% stringr::str_to_upper(c("NSCLC", "PANC", "BLADDER", "PROSTATE"))) {
        c("DFCI", "MSK", "UHN", "VICC")
      } else if (stringr::str_to_upper(cohort_temp[[x]]) %in% c("CRC", "BRCA")) {
        c("DFCI", "MSK", "VICC")
      }
    })
  }

  # to account for unspecified stage

  if (is.null(stage_dx)) {
    stage_dx_temp <- purrr::map(data_synapse, function(x) {
      pull(pluck(x, "ca_dx_index") %>%
        dplyr::distinct(stage_dx), stage_dx)
    })
  } else {
    stage_dx_temp <- purrr::map(data_synapse, ~ stage_dx)
  }

  # stage mis-specified
  if (class(stage_dx) == "list"){
    stop("`stage_dx` must be a vector, not a list. Note that stage(s)
        listed apply to all cohorts in the data_synapse object.")
  } else if (!is.null(stage_dx) &&
    any(!grepl(
      c("^stage i$|^stage ii$|^stage iii$|
                 ^stage i-iii nos$|^stage iv$"),
      stringr::str_to_lower(stage_dx)))) {
    stop("Select from available stages: Stage I, Stage II, Stage III,
         Stage I-III NOS, Stage IV")
  }

  histology_temp <- if (!is.null(histology)) {
    # is histology mis-specified?
    purrr::map(1:length(data_synapse), function(x) {
      if (class(histology) == "list"){
        stop("`histology` must be a vector, not a list. Note that histologies
        listed apply to all cohorts in the data_synapse object.")
      } else if (cohort_temp[[x]] != "BrCa" &&
        any(!grepl(
          c(
            "^adenocarcinoma$|^squamous cell$|^sarcoma$|^small cell
                 carcinoma$|^carcinoma$|^other histologies/mixed tumor$"
          ),
          stringr::str_to_lower(histology)))) {
        stop(
          "Select from available histology categories: Adenocarcinoma,
         Squamous cell, Sarcoma, Small cell carcinoma, Other histologies/mixed
         tumor"
        )
      } else if (cohort_temp[[x]] == "BrCa" &&
        any(!grepl(
          c(
            "^invasive lobular carcinoma$|^invasive ductal carcinoma$|
                 ^Other histology$"
          ),
          histology
        ))) {
        stop(
          "Select from available histology categories: Invasive lobular
         carcinoma, Invasive ductal carcinoma, Other histology"
        )
      } else {
        histology
      }
    })
  } else {
    purrr::map(1:length(data_synapse), function(x) {
      if (cohort_temp[[x]] != "BrCa") {
        pull(
          pluck(data_synapse[[x]], "ca_dx_index") %>%
            distinct(.data$ca_hist_adeno_squamous),
          .data$ca_hist_adeno_squamous
        )
      } else {
        pull(
          pluck(data_synapse[[x]], "ca_dx_index") %>%
            distinct(.data$ca_hist_brca),
          "ca_hist_brca"
        )
      }
    })
  }

  ### drug regimen parameter checks
  # if regimen type is mis-specified
  if (!is.null(regimen_type) | is.numeric(regimen_type)) {
    if (!(stringr::str_to_lower(regimen_type) %in% c("exact", "containing"))) {
      stop("For regimen_type select from 'exact' or 'containing'")
    }
  }

  # if regimen_order is not numeric
  if (!is.null(regimen_order) && !is.numeric(regimen_order)) {
    stop("The regimen_order parameter must be a numeric value >=1.")
  }

  # if regimen_order_type is mis-specified
  if (!is.null(regimen_order_type) &&
    (is.numeric(regimen_order_type) ||
      !(stringr::str_to_lower(regimen_order_type) %in% c(
        "within cancer",
        "within regimen"
      )))) {
    stop("For regimen_order_type select from 'within cancer' or
         'within regimen'")
  }

  # regimen_order_type needs to be specified if regimen_order is specified
  if (is.null(regimen_order_type) && !is.null(regimen_order)) {
    stop("Regimen order type must also be specified. Choose from
         'within cancer' or 'within regimen'")
  }

  # can't only specify regimen_order_type
  if (!is.null(regimen_order_type) && is.null(regimen_order)) {
    stop("Numeric order must also be specified in 'regimen_order' argument.")
  }

  # if regimen_type is specified, regimen_drugs must also be specified
  if (regimen_type != "Exact" && is.null(regimen_drugs)) {
    stop("If regimen_type is specified, regimen_drugs must also be specified.")
  }

  if (is.null(regimen_order_type)) {
    regimen_order_type <- NULL
  }

  ##############################################################################
  #                             pull cancer cohort                             #
  ##############################################################################

  # select patients based on cohort, institution, stage at diagnosis,
  # histology and cancer number
  cohort_ca_dx <- purrr::map(1:length(data_synapse), function(x) {
    if (cohort_temp[[x]] != "BrCa") {
      pluck(data_synapse[[x]], "ca_dx_index") %>%
        # re-number index cancer diagnoses
        dplyr::group_by(.data$cohort, .data$record_id) %>%
        dplyr::mutate(index_ca_seq = 1:n()) %>%
        dplyr::ungroup() %>%
        # apply filter(s)
        dplyr::filter(
          stringr::str_to_lower(.data$institution) %in%
            stringr::str_to_lower(unlist(institution_temp[[x]])),
          stringr::str_to_lower(.data$stage_dx) %in%
            stringr::str_to_lower(unlist(stage_dx_temp[[x]])),
          stringr::str_to_lower(.data$ca_hist_adeno_squamous) %in%
            stringr::str_to_lower(unlist(histology_temp[[x]])),
          .data$index_ca_seq %in% c({{ index_ca_seq }})
        )
    } else {
      pluck(data_synapse[[x]], "ca_dx_index") %>%
        # re-number index cancer diagnoses
        dplyr::group_by(.data$cohort, .data$record_id) %>%
        dplyr::mutate(index_ca_seq = 1:n()) %>%
        dplyr::ungroup() %>%
        # # apply filter(s)
        dplyr::filter(
          stringr::str_to_lower(.data$institution) %in%
            stringr::str_to_lower(c(institution_temp[[x]])),
          stringr::str_to_lower(.data$stage_dx) %in%
            stringr::str_to_lower(c(stage_dx_temp[[x]])),
          stringr::str_to_lower(.data$ca_hist_brca) %in%
            stringr::str_to_lower(c(histology_temp[[x]])),
          .data$index_ca_seq %in% c({{ index_ca_seq }})
        )
    }
  })

  # only continue if patients met the inclusion criteria
  if (any(map(cohort_ca_dx, nrow) > 0)){

    # browser()

  # only map over cohorts that have patients that met the criteria
  index_pts_meeting_criteria <- which((map_int(cohort_ca_dx, nrow) > 0))
  cohort_ca_dx_incl <- cohort_ca_dx[index_pts_meeting_criteria]
  data_synapse_incl <- data_synapse[index_pts_meeting_criteria]

  # pull drug regimens to those patients
  # option 1: all drug regimens to all patients in cohort
  # regimen_drugs is not specified, regimen_order is not specified
  cohort_ca_drugs <- purrr::map2(cohort_ca_dx_incl, data_synapse_incl,
                                 ~dplyr::inner_join(
      .x %>%
        dplyr::select("cohort", "record_id", "ca_seq"),
      pluck(.y, "ca_drugs"),
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
        pluck(.y, "ca_drugs") %>%
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
  )

  # option 2: all "first line" drug regimens (regimens of a certain number,
  # within a cancer diagnosis)
  # specific regimen number to all pts in cohort, any regimen name
  # regimen_drugs is not specified, regimen_order is specified and
  # regimen_type = "within cancer"
  if (is.null(regimen_drugs) && !is.null(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {

      cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
        cohort_ca_drugs[[x]] %>%
        dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }}))
      })

      # restrict cancer cohort to all patients who got a drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          dplyr::filter(.data$order_within_cancer %in% c({{ regimen_order }})) %>%
          dplyr::select("cohort", "record_id", "institution", "ca_seq"),
        by = c(
          "cohort", "record_id", "institution", "ca_seq"
        )
      )
    })
  }


  # if specific drug regimen is requested; exact regimen
  # option 3a: all times that exact drug regimen was received
  if (!is.null(regimen_drugs) && is.null(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "exact") {
    # identify instances of that drug regimen
    cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
      cohort_ca_drugs[[x]] %>%
        dplyr::filter(
          str_to_lower(.data$regimen_drugs) %in% c(regimen_drugs_sorted) |
            str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted) #|
          # drug_class %in% c(regimen_drugs_sorted)
        )})

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          dplyr::distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }

  # option 3b: all times that regimen containing drugs was received

  if (!is.null(regimen_drugs) && is.null(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "containing") {
    cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
      # identify instances of that drug regimen
       cohort_ca_drugs[[x]] %>%
        dplyr::filter(grepl(
          paste(regimen_drugs_sorted, collapse = "|"),
          str_to_lower(.data$regimen_drugs)
        ) |
          grepl(
            paste(regimen_drugs_sorted, collapse = "|"),
            str_to_lower(.data$abbreviation)
          ))})

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          dplyr::distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }

  # option 4a: 1st (or other) time that exact regimen was received
  if (!is.null(regimen_drugs) && !is.null(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within regimen" &&
    stringr::str_to_lower(regimen_type) == "exact") {

      # identify instances of that drug regimen
      cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
        cohort_ca_drugs[[x]] %>%
        dplyr::filter(str_to_lower(.data$regimen_drugs)
        %in% c(regimen_drugs_sorted) |
          str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted)) %>%
        # filter on order of interest (e.g. first, all)
        dplyr::filter(.data$order_within_regimen %in% c({{ regimen_order }}))
      })

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }

  # option 4b: 1st (or other) time that regimen containing was received
  if (!is.null(regimen_drugs) &&
    !is.null(regimen_order) &&
    stringr::str_to_lower(regimen_order_type) == "within regimen" &&
    stringr::str_to_lower(regimen_type) == "containing") {

      # identify instances of that drug regimen
      # have to start with full drugs dataset for 'within regimen',
      # otherwise are left with all drug regimens to pts in this cohort
      cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
        pluck(data_synapse_incl[[x]], "ca_drugs") %>%
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
          pluck(data_synapse_incl[[x]], "ca_drugs") %>%
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
            # potentially purrr::mapped to multiple ca types)
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
          cohort_ca_dx[[x]] %>%
            dplyr::select("cohort", "record_id", "ca_seq"),
          by = c("cohort", "record_id", "ca_seq")
        ) %>%
        # create blank variables (dropped below, not having them is unique to
        # regimen_order_type = 'containing')
        mutate(
          order_within_cancer = as.numeric(NA),
          order_within_regimen = as.numeric(NA)
        )
      })

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          dplyr::distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }

  # option 5a: specific drugs within a cancer diagnosis, exact regimen

  if (!is.null(regimen_drugs) &&
    !is.null(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "exact" &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {

      # identify instances of that drug regimen
      cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
        cohort_ca_drugs[[x]] %>%
        dplyr::filter(
          str_to_lower(.data$regimen_drugs) %in% c(regimen_drugs_sorted) |
            str_to_lower(.data$abbreviation) %in% c(regimen_drugs_sorted),
          .data$order_within_cancer %in% c({{ regimen_order }})
        )
      })

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }


  # option 5b: specific drugs within a cancer diagnosis, regimen containing
  if (!is.null(regimen_drugs) &&
    !is.null(regimen_order) &&
    stringr::str_to_lower(regimen_type) == "containing" &&
    stringr::str_to_lower(regimen_order_type) == "within cancer") {

      # identify instances of that drug regimen
      cohort_ca_drugs <- purrr::map(1:length(data_synapse_incl), function(x) {
        cohort_ca_drugs[[x]] %>%
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
      })

      # restrict cancer cohort to patients on that drug regimen
      cohort_ca_dx <- purrr::map(1:length(data_synapse_incl), function(x) {
        dplyr::inner_join(cohort_ca_dx[[x]],
        cohort_ca_drugs[[x]] %>%
          dplyr::distinct(
            .data$cohort, .data$record_id, .data$institution,
            .data$ca_seq
          ),
        by = c("cohort", "record_id", "institution", "ca_seq")
      )
    })
  }

  # for patients meeting the specified criteria, also pull related datasets
  # for each dataframe returned by pull_data_synapse, return the data
  # subset to match the specified cohort
  subset_all_dfs <- purrr::map(index_pts_meeting_criteria, function(x) {
    # get specific datasets for each cohort/data release in data_synapse
    genie_dfs <- names(data_synapse[[x]])

    browser()

    # get cohort and version corresponding to data release to add to all returned datasets
    # variable release_version is on clinical datasets, but not on genomic data
    # so when create_analytic_cohort is called for multiple cohorts and the
    # resulting data is stacked together, need to be able to distinguish btwn
    # data releases
    cohort <- word(names(data_synapse[x]), 1, sep = "_")
    version <- word(names(data_synapse[x]), 2, sep = "_")

    # browser()
    # then for each of those datasets, subset on the patients of interest
    purrr::map(genie_dfs, function(df_name) {
      # datasets that don't require subsetting (cancer dx + ca_drugs) because
      # they were already processed
      if (df_name %in% c("ca_dx_index")){
        cohort_ca_dx[[x]] %>%
          dplyr::select(-"index_ca_seq") %>%
          dplyr::mutate(version = version)
      } else if (df_name %in% c("ca_drugs")) {
        cohort_ca_drugs[[x]] %>%
          dplyr::select(-"order_within_cancer",
                        -"order_within_regimen",
                        -"abbreviation") %>%
          dplyr::mutate(version = version)

        # datasets that get can be subset by record_id
        } else if (df_name %in% c("pt_char", "ca_dx_non_index",
                         "prissmm_imaging", "prissmm_pathology",
                         "prissmm_md", "tumor_marker") &
          !is.null(pluck(data_synapse[[x]], df_name))) {
        dplyr::inner_join(cohort_ca_dx[[x]] %>%
                             dplyr::select("cohort", "record_id"),
                           pluck(data_synapse[[x]], df_name),
                           by = c("cohort", "record_id")) %>%
            dplyr::mutate(version = version)
        # datasets that require merging by cohort, record_id, ca_seq
      } else if (df_name %in% c("ca_radtx") &
                 !is.null(pluck(data_synapse[[x]], df_name))) {
        dplyr::inner_join(cohort_ca_dx[[x]] %>%
                            dplyr::select("cohort", "record_id", "ca_seq"),
                          pluck(data_synapse[[x]], df_name),
                          by = c("cohort", "record_id", "ca_seq")) %>%
          dplyr::mutate(version = version)
      } else if (df_name == "cpt") {
        # cancer panel test information
        # keep records based on record_id + cancer sequence of interest
        cpt <- dplyr::inner_join(
          cohort_ca_dx[[x]] %>%
            dplyr::select("cohort", "record_id", "ca_seq"),
          pluck(data_synapse[[x]], df_name),
          by = c("cohort", "record_id", "ca_seq")
        ) %>%
          dplyr::mutate(version = version)

        # if variable sample_type isn't on the NGS dataframe (earlier data
        # releases), define it here
        if (any(names(cpt) == "cpt_sample_type") &
          !any(names(cpt) == "sample_type")) {
          cpt %>%
            dplyr::mutate(
              sample_type = case_when(
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
              )
            )
        } else {
          cpt
        }
        } else if (df_name %in% c("cna")) {
        # cna file is 1 col / tumor sample barcode
        # all genes are first column and every name is a column title.
        if (!is.null(pluck(data_synapse[[x]], df_name))) {
          # get list of IDs to keep
          cpt_barcode_keep <- pluck(data_synapse[[x]], "cpt") %>%
            mutate(
              Tumor_Sample_Barcode =
                stringr::str_replace_all(.data$cpt_genie_sample_id,
                  pattern = "-",
                  replacement = "\\."
                )
            ) %>%
            pull("Tumor_Sample_Barcode")

          pluck(data_synapse[[x]], df_name) %>%
            select("Hugo_Symbol", any_of(cpt_barcode_keep)) %>%
            dplyr::mutate(cohort = cohort,
                          version = version)

        }

          # browser()
      } else if (df_name %in% c("fusions", "mutations_extended") &&
        !is.null(pluck(data_synapse[[x]], df_name))) {
        dplyr::inner_join(
          pluck(data_synapse[[x]], df_name),
          pluck(data_synapse[[x]], "cpt") %>%
            dplyr::select("cohort", "cpt_genie_sample_id") %>%
            distinct(),
          by = c("Tumor_Sample_Barcode" = "cpt_genie_sample_id")
        ) %>%
          dplyr::mutate(cohort = cohort,
                        version = version)
      } else if (df_name %in% c("sv") &&
                 !is.null(pluck(data_synapse[[x]], df_name))) {
        # get list of IDs to keep
        cpt_barcode_keep <- pluck(data_synapse[[x]], "cpt") %>%
          mutate(
            Tumor_Sample_Barcode =
              stringr::str_replace_all(.data$cpt_genie_sample_id,
                                       pattern = "-",
                                       replacement = "\\."
              )
          ) %>%
          pull("Tumor_Sample_Barcode")

        # subset to those IDs
        dplyr::inner_join(
          pluck(data_synapse[[x]], df_name),
          pluck(data_synapse[[x]], "cpt") %>%
            dplyr::select("cohort", "cpt_genie_sample_id") %>%
            distinct(),
          by = c("Sample_Id" = "cpt_genie_sample_id")
        ) %>%
          dplyr::mutate(cohort = cohort,
                        version = version)
      }
    })
  })

  # name each cohort
  names(subset_all_dfs) <- unlist(cohort_temp[index_pts_meeting_criteria])
} # end of if statement for processing cohort only if patients met cancer
  # diagnosis dataset inclusion criteria

  # browser()

  # if 0 patients are returned for any cancer cohort, print a message and
  # proceed with processing
  # create vector to store indexes of cohorts with zero records
  zero_pts_returned <- purrr::map(cohort_ca_dx, ~ nrow(.) == 0)

  # stop if no patients returned for any cohort
  if (!(FALSE %in% zero_pts_returned)){
    stop(paste0(
      "No patients meeting the specified criteria were returned. Ensure that all parameters were correctly specified and check the raw data returned by the pull_data_synapse() call to ensure that there are patients that met the specified criteria (e.g., that there were patients with the specified combination of cancer type, institution, histology, stage, etc.). Additionally, the list of acceptable drugs can be found in the `drug_regimen_list` dataset available within this package. Note that drug names may vary across cancer cohorts and data releases."
    ))

  }

  # if no patients returned for some cohorts, print message and proceed
  if (TRUE %in% zero_pts_returned) {
    message_coh <- cohort_temp[unlist(zero_pts_returned)] %>%
      unlist() %>%
      paste(sep = "/", collapse = ", ")

    message(paste0(
      "No patients meeting the specified criteria were returned for the ",
      message_coh, " cohort(s). Data for other cohorts is included in the analytic cohort."
    ))
  }


  # apply names to objects that are part of subset_all_dfs
  # rename ca_dx_index to ca_dx (cohort prefix added for all dfs below)
  for (i in seq_along(subset_all_dfs)) {
    names(subset_all_dfs[[i]]) <- str_replace_all(string = names(data_synapse_incl[[i]]),
                                                  pattern = c("ca_dx_index" = "ca_dx"))
  }

  # combine datasets across cancer cohorts
  # for variables with conflicting data types, convert to character
  # get list of all datasets included across cancer cohorts
  df_list_pan_can <- unique(purrr::simplify(purrr::map(subset_all_dfs, names)))

  # for each dataframe included across cancer cohorts
  # obtain the corresponding dataframe for each particular cancer type
  # some variables are different data types across data releases, make character
  # to allow them to be set together
  data_return <- map(df_list_pan_can, function(name_df){
    map(subset_all_dfs, pluck, name_df) %>%
      purrr::compact() %>%
      list_flatten() %>%
      map(., ~mutate(.x,
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
      bind_rows()
  }
  )

  # name datasets with "cohort_" + df name
  # rename cpt to ngs
  names(data_return) <- stringr::str_replace(
    pattern = "cpt",
    replacement = "ngs",
    string = paste0("cohort_", stringr::str_remove(pattern = "cohort_",
                                                     string = df_list_pan_can)))

  browser()

  # warn if >1 data release per cohort
  chk_n_release_coh <- data_return %>%
    purrr::pluck("cohort_pt_char") %>%
    dplyr::count(.data$cohort, .data$version) %>%
    dplyr::count(.data$cohort) %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(.data$cohort)

  if (length(chk_n_release_coh) > 1){
    cli::cli_warn("When creating an analytic cohort, it is recommended to use the most recent data release(s). It is not advisable to base an analytic cohort on multiple data releases of the same cancer cohort ({.val {paste0(chk_n_release_coh, collapse = ', ')}}).")
  }


  # return a table 1 to describe the cancer cohort if the user specifies
  if (nrow(data_return %>% pluck("cohort_ca_dx")) > 0 && return_summary == TRUE) {
    # number of records per patient in the diagnosis dataset
    n_rec_dx_dset <- data_return %>%
      pluck("cohort_ca_dx") %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = "n_rec_pt",
        label = n_rec_pt ~ "Number of diagnoses per patient in cohort_ca_dx
        data frame",
        type = n_rec_pt ~ "categorical"
      ) %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} patients**"
        ),
        quiet = TRUE
      )

    n_rec_drugs_dset <- data_return %>%
      pluck("cohort_ca_drugs") %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::summarize(n_rec_pt = n(), .groups = "drop") %>%
      gtsummary::tbl_summary(
        include = "n_rec_pt",
        label = n_rec_pt ~ "Number of regimens per patient in cohort_ca_drugs
        data frame",
        type = n_rec_pt ~ "categorical"
      )

    n_rec_cpt_dset <- data_return %>%
      pluck("cohort_ngs") %>%
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

    if (!("BrCa" %in% cohort_temp)) {
      tbl_cohort <- data_return %>%
        pluck("cohort_ca_dx") %>%
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
        gtsummary::modify_header(
          update = list(
            stat_0 ~ "**N = {N} Diagnoses**"
          ),
          quiet = TRUE
        )
    } else {
      tbl_cohort <- data_return %>%
        pluck("cohort_ca_dx") %>%
        # dplyr::group_by(.data$record_id) %>%
        # dplyr::mutate(n_rec_pt = n()) %>%
        # dplyr::ungroup() %>%
        gtsummary::tbl_summary(
          include = c(
            "cohort", "institution", "stage_dx",
            "ca_hist_adeno_squamous",
            "ca_hist_brca"
          ),
          label = list(
            cohort ~ "Cohort (cohort)",
            institution ~ "Institution (institution)",
            stage_dx ~ "Stage at diagnosis (stage_dx)",
            ca_hist_brca ~ "Histology (ca_hist_brca)"
          )
        ) %>%
        gtsummary::bold_labels() %>%
        gtsummary::modify_header(
          update = list(
            stat_0 ~ "**N = {N} Diagnoses**"
          ),
          quiet = TRUE
        )
    }

    tbl_drugs <- data_return %>%
      pluck("cohort_ca_drugs") %>%
      gtsummary::tbl_summary(
        include = c("cohort", "institution", "regimen_drugs"),
        label = list(
          cohort ~ "Cohort (cohort)",
          institution ~ "Institution (institution)",
          regimen_drugs ~ "Drugs in regimen (regimen_drugs)"
        )
      ) %>%
      gtsummary::bold_labels() %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} Regimens**"
        ),
        quiet = TRUE
      )

    tbl_ngs <- data_return %>%
      pluck("cohort_ngs") %>%
      gtsummary::tbl_summary(
        include = c(
          "cohort", "institution",
          "cpt_oncotree_code", "cpt_seq_assay_id"
        ),
        label = list(
          cohort ~ "Cohort (cohort)",
          institution ~ "Institution (institution)",
          cpt_oncotree_code ~ "OncoTree code (cpt_oncotree_code)",
          cpt_seq_assay_id ~ "Sequence assay ID (cpt_seq_assay_id)"
        )
      ) %>%
      gtsummary::bold_labels() %>%
      gtsummary::modify_header(
        update = list(
          stat_0 ~ "**N = {N} Cancer Panel Tests**"
        ),
        quiet = TRUE
      )

    data_return$tbl_overall_summary <- tbl_overall_summary
    data_return$tbl_cohort <- tbl_cohort
    data_return$tbl_drugs <- tbl_drugs
    data_return$tbl_ngs <- tbl_ngs
  }

  # drop variable before returning data frame


  # order of dataframes, should they exist
  df_order <- c(
    "cohort_pt_char", "cohort_ca_dx",
    "cohort_ca_dx_non_index",
    "cohort_ca_radtx", "cohort_ca_drugs",
    "cohort_prissmm_imaging", "cohort_prissmm_pathology",
    "cohort_prissmm_md", "cohort_tumor_marker",
    "cohort_ngs",
    "cohort_mutations_extended",
    "cohort_fusions", "cohort_sv",
    "cohort_cna",
    "tbl_overall_summary", "tbl_cohort", "tbl_drugs", "tbl_ngs"
  )

  # save elements on list in order that we want (clinical datasets, genomic
  # datasets, tables) and drop any items that don't appear in this run of
  # create_analytic_cohort
  data_return <- data_return[c(df_order)] %>%
    purrr::compact()


  if (nrow(data_return$cohort_ca_dx) > 0) {
    return(data_return)
  }
  #} # end of else statement to proceed when patients are returned that met the criteria
} # end of function
