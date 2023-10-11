#' Selecting corresponding unique next generation sequencing reports
#'
#' For patients with multiple associated next generation (NGS) sequencing
#' reports, select one unique NGS report per patient for the purpose of creating
#' an analytic dataset based on user-defined criterion, including OncoTree code,
#' primary vs. metastatic tumor sample, and earliest vs. most recent sample. If
#' multiple reports for a patient remain available after the user-defined
#' specifications, or if no specifications are provided, the panel with the
#' largest number of genes is selected by default. Sample optimization is
#' performed in the order that the arguments are specified in the function,
#' regardless of the arguments’ order provided by the user. Namely, the OncoTree
#' code is prioritized first, sample type is prioritized second and finally the
#' time is prioritized last. For patients with exactly one genomic sample, that
#' unique genomic sample will be returned regardless of whether it meets the
#' user-specified parameters. Running the select_unique_ngs() function will
#' ensure that the resulting dataset returned by merging the next generation
#' sequencing report data onto the cohort_ca_dx dataset returned by
#' create_analytic_cohort() will maintain the structure of cohort_ca_dx (either
#' one record per patient or one record per diagnosis). Currently, if multiple
#' diagnoses per patient are returned from create_analytic_cohort(), using
#' select_unique_ngs() will select a single NGS report per patient. In future
#' iterations, this will be updated so that one NGS report per diagnosis can be
#' selected.
#'
#' Note that the NGS dataset serves as the link between the clinical and
#' genomic data, where the NGS dataset includes one record per NGS report per
#' patient, including the NGS sample ID that is used to link to the genomic
#' data files. Merging data from the NGS report onto the analytic cohort
#' returned from create_analytic_cohort() therefore allows users to utilize all
#' clinical and genomic data available.
#'
#' See the
#' \href{https://genie-bpc.github.io/genieBPC/articles/select_unique_ngs_vignette.html}{select_unique_ngs vignette}
#' for further documentation and examples.
#'
#' @param data_cohort output object of the create_analytic_cohort function.
#' @param oncotree_code character vector specifying which sample
#' OncoTree codes to keep. See "cpt_oncotree_code" column
#' of data_cohort argument above to get options.
#' @param sample_type character specifying which type of genomic sample
#' to prioritize, options are "Primary", "Local" and "Metastasis".
#' Default is to not select a NGS sample based on the sample type.
#' @param min_max_time character specifying if the first or last genomic
#' sample recorded should be kept.
#' Options are "min" (first) and "max" (last).
#'
#' @return returns the 'cohort_ngs' object of the create_analytic_cohort
#' with unique genomic samples taken from each patients.
#' @author Karissa Whiting
#' @export
#'
#' @examplesIf genieBPC::.is_connected_to_genie()
#' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC of
#' # histology adenocarcinoma
#' nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#'
#' ex1 <- create_analytic_cohort(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   stage_dx = c("Stage IV"),
#'   histology = "Adenocarcinoma"
#' )
#'
#' # select unique next generation sequencing reports for those patients
#' samples_data1 <- select_unique_ngs(
#'   data_cohort = ex1$cohort_ngs,
#'   sample_type = "Primary"
#' )
#'
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who
#' # received Cisplatin, Pemetrexed Disodium or Cisplatin,
#' # Etoposide as their first drug regimen
#' ex2 <- create_analytic_cohort(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   regimen_drugs = c(
#'     "Cisplatin, Pemetrexed Disodium",
#'     "Cisplatin, Etoposide"
#'   ),
#'   regimen_order = 1,
#'   regimen_order_type = "within regimen"
#' )
#'
#' samples_data2 <- select_unique_ngs(
#'   data_cohort = ex2$cohort_ngs,
#'   oncotree_code = "NSCLCPD",
#'   sample_type = "Metastasis",
#'   min_max_time = "max"
#' )
#'
#' @import
#' dplyr
#' dtplyr
#' tibble
#'
select_unique_ngs <- function(data_cohort,
                              oncotree_code = NULL,
                              sample_type = NULL,
                              min_max_time = NULL) {

  # perform checks #
  if (missing(data_cohort)) {
    cli::cli_abort(
      "The 'data_cohort' argument is needed to perform this process. 'data_cohort' is the list object 'cohort_cpt' returned from the create_analytic_cohort() function.")
  }

  if (!is.null(min_max_time) && (sum(!min_max_time %in% c("min", "max")) > 0 ||
    as.numeric(length(min_max_time)) > 1)) {
    cli::cli_abort("The 'min_max_time' argument should be either 'min' or 'max' (only one of the two).")
  }

  if (!is.null(sample_type) &&
    (length(sample_type) > 1 ||
      !(stringr::str_to_lower(sample_type) %in%
        c("primary", "local", "metastasis")))) {
    cli::cli_abort(
      "Please input a single sample of type of interest out of 'Primary', 'Local' or 'Metastasis'")
  }

  # if(sum(grepl("samples_data",names(data_cohort))) != 1)
  #   stop("The 'data_cohort' input did not contain the 'samples_data' object.
  if (is.null(oncotree_code) && is.null(sample_type) && is.null(min_max_time)) {
    cli::cli_alert_warning("None of the optimization arguments were specified. The sample with the largest panel size will be returned. In the case of ties a random sample will be returned.")
  }

  if (!is.null(oncotree_code) &&
    sum(data_cohort$cpt_oncotree_code %in% oncotree_code) == 0) {
    cli::cli_alert_warning(
      "The OncoTree code inputted does not exist in the data and will be ignored. OncoTree codes will not be used to select a unique next generation sequencing panel for each patient.")

    oncotree_code <- NULL
  }

  # Perform the selection only for patients that have multiple samples #

  ### Find patients that had duplicated samples ###
  dup_samples <- as.character(unlist(data_cohort %>%
    group_by(.data$record_id) %>%
    summarise(
      N_samples =
        length(unique(.data$cpt_genie_sample_id))
    ) %>%
    filter(.data$N_samples > 1) %>%
    select("record_id")))


  dedupe_res <- map(dup_samples, ~.resolve_duplicates(.x,
                                        data_cohort = data_cohort,
                                        oncotree_code = oncotree_code,
                                        sample_type = sample_type,
                                        min_max_time = min_max_time))

  solved_dups <- map_df(dedupe_res, function(item) {
    item$data
  })

   messages_type <- map_chr(dedupe_res, function(item) {
     item$no_sample_type
   }) %>% .[!is.na(.)]

   messages_source <- map_chr(dedupe_res, function(item) {
     item$no_sample_source
   }) %>% .[!is.na(.)]

   messages_random <- map_chr(dedupe_res, function(item) {
     item$random
   }) %>% .[!is.na(.)]


  # remove all patients with duplicates and add back their selected samples #
  samples_data_final <-     data_cohort %>%
      filter(!(.data$record_id %in% dup_samples)) %>%
      bind_rows(solved_dups)

  if(length(dup_samples > 0)) {
    cli::cli_alert_success(
      c("{.field {length(dup_samples)}} patients with > 1 next generation sequencing reports were identified"))
  }
  if (!(is.null(oncotree_code) && is.null(sample_type) && is.null(min_max_time))) {
#
#     if(length(messages_type) > 0) {
#       attr(samples_data_final, "no_sample_of_type") <- messages_type
#       cli::cli_alert_warning(c("{length(messages_type)} patients had no
#       samples matching {.field sample_type = {sample_type}}, ",
#       "therefore other criteria was used to select a final sample
#       (see {.code ?select_unique_ngs}). ",
#       "See {.code attributes(<your-results>)$no_sample_of_type}
#       to view these sample IDs."))
#     }
#     if(length(messages_source) > 0) {
#       attr(samples_data_final, "no_sample_of_source") <- messages_source
#       cli::cli_alert_warning(c("{length(messages_source)} patients had no
#       samples matching {.field oncotree_code = {oncotree_code}}, ",
#       "therefore other criteria was used to select a final sample
#       (see {.code ?select_unique_ngs}). ",
#       "See {.code attributes(<your-results>)$no_sample_of_source} to view
#       these sample IDs."))
#     }

    if(length(messages_random) > 0) {
      attr(samples_data_final, "random_samples") <- messages_random

      dedup_by_criteria <- length(dup_samples) -length(messages_random)
      cli::cli_alert_success(c("{.val {dedup_by_criteria}} of {.field {length(dup_samples)}} had a unique NGS report selected based on given criteria."))

      cli::cli_alert_warning(c("{.val {length(messages_random)}} of {.field {length(dup_samples)}} did not have a unique NGS report selected based on the selected criteria or by having the largest panel, so",
      " a NGS report was selected at random (be sure to set a seed for reproducbility!) ",
      "See {.code attributes(<your-results>)$random_samples} to view these sample IDs."))
    }
  }


  return(samples_data_final)
}


#' Select unique NGS report when multiple are available
#'
#' See `select_unique_ngs` for details on selection criteria
#' @param x sample ID to select unique NGS report for
#' @inheritParams select_unique_ngs
#' @keywords internal
#' @return a dataframe of samples with one observation per patient.
#' @export
#'
#' @examplesIf genieBPC::.is_connected_to_genie()
#' nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#'
#' ex1 <- create_analytic_cohort(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   stage_dx = c("Stage IV"),
#'   histology = "Adenocarcinoma"
#' )
#'
#'  samples_data1 <- .resolve_duplicates(
#'    x = "GENIE-MSK-P-0025741",
#'   data_cohort = ex1$cohort_ngs,
#'   oncotree_code = "LUAD",
#'   sample_type = "Metastasis",
#'   min_max_time = "max"
#' )
#'
#' samples_data2 <- .resolve_duplicates(
#'    x = "GENIE-MSK-P-0025741",
#'   data_cohort = ex1$cohort_ngs,
#'   oncotree_code = "LUAD",
#'   sample_type = "Primary",
#'   min_max_time = "max"
#' )
#'
.resolve_duplicates <- function(x, data_cohort,
                                oncotree_code,
                                sample_type,
                                min_max_time) {

  no_sample_source <- NA
  no_sample_type <- NA
  random <- NA

  temp <- data_cohort %>%
    filter(.data$record_id == x)

  # Oncotree code -----
  if (!is.null(oncotree_code)) {

    if(sum(temp$cpt_oncotree_code %in% oncotree_code) > 0) {

    temp <- temp %>%
      filter(.data$cpt_oncotree_code %in% oncotree_code)

    # if none of that type
    } else {
      no_sample_source <- x

  }
}

  # Sample type ----
  if (!is.null(sample_type)) {

    if(sum(grepl(sample_type, temp$sample_type, ignore.case = TRUE  )) > 0) {

      temp <- temp[grepl(sample_type, temp$sample_type, ignore.case = TRUE), ]

    } else {
      no_sample_type <- x
    }
  }



  # Min/Max time ----
  if (!is.null(min_max_time) & nrow(temp) > 1) {
    if (min_max_time == "min") {
      temp <- temp %>%
        filter(.data$dx_cpt_rep_days == min(.data$dx_cpt_rep_days))
    }
    if (min_max_time == "max") {
      temp <- temp %>%
        filter(.data$dx_cpt_rep_days == max(.data$dx_cpt_rep_days))
    }
  }

  # Panel number ------
  # If there are still multiple samples, then
  # select the sample with largest panel number
  if (nrow(temp) > 1 ) {

    temp <- temp %>%
      filter(.data$cpt_seq_assay_id %in%
               genieBPC::genie_panels$Sequence.Assay.ID) %>%
      rowwise() %>%
      mutate(
        panel_size =
          genieBPC::genie_panels[
            match(
              .data$cpt_seq_assay_id,
              genieBPC::genie_panels$Sequence.Assay.ID
            ),
            "Genes"
          ]
      ) %>%
      ungroup() %>%
      filter(.data$panel_size == max(.data$panel_size)) %>%
      select(-one_of("panel_size"))
  }

  # If somehow there are still multiple possible samples, then
  # pick one at random
  if (nrow(temp) > 1) {

    random <- x
    temp <- temp[sample(seq_along(temp[, 1]), size = 1), ]
  } else {
    random <- NA
  }


  return(list("data" = temp,
              "no_sample_source" = no_sample_source,
              "no_sample_type" = no_sample_type,
              "random" = random))
}
