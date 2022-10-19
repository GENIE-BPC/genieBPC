#' fetch_samples
#'
#' This function links patients in a cohort (created by
#' create_analytic_cohort()) with their corresponding
#' genomic samples available in GENIE.
#'
#' Subset cancer panel test data to patients in the cohort of interest
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC", "CRC",
#' or "BrCa"
#' @param data_synapse The item from the nested list returned from
#' `pull_data_synapse()`
#' @param df_record_ids NGS data frame from the `create_analytic_cohort()`
#'   function.
#' @return returns the 'cohort_ngs' object of the create_analytic_cohort
#' with the genomic samples taken from each patients.
#' @author Axel Martin
#' @import
#' dplyr
#' dtplyr
#' tibble

fetch_samples <- function(cohort, data_synapse, df_record_ids) {
  # get `cohort` ---
  cohort_temp <- rlang::arg_match(cohort, c("NSCLC", "CRC", "BrCa"),
                                    multiple = FALSE
  )


  if (missing(df_record_ids)) {
    stop("You must provide an object that contains the variable record_id.")
  }

  if (sum(grepl("record_id", names(df_record_ids))) < 1) {
    stop("You must provide an object that contains a variable called
         record_id.")
  }

  # keep records based on patient ID + cancer sequence of interest
  cohort_cpt <- dplyr::inner_join(df_record_ids %>%
    dplyr::select("cohort", "record_id", "ca_seq"),
  pluck(data_synapse, "cpt"),
  by = c("cohort", "record_id", "ca_seq")
  ) %>%
    distinct() %>%
    purrr::when(
      # if older release and cpt_sample_type is available, but derived
      # variable sample_type is not
      (any(names(.) == "cpt_sample_type") &
        !any(names(.) == "sample_type")) ~ dplyr::mutate(.,
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
      ),
      TRUE ~ .
    )

  return(cohort_cpt)
}
