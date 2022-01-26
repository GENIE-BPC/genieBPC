#' fetch_samples
#'
#' Subset cancer panel test data to patients in the cohort of interest
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC", "CRC",
#' or "BrCa"
#' @param data_synapse the list object outputted by the `pull_data_synapse()` function.
#' @param df_record_ids output object of the create_cohort function.
#' @return returns the cohort object list inputted with an additional dataset named "samples_data".
#' @export
#' @author Axel Martin
#' #' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC of histology adenocarcinoma
#' out <- create_cohort(cohort = "NSCLC",
#'      stage_dx = c("Stage IV"),
#'      ca_hist_adeno_squamous = "Adenocarcinoma")
#' samples_data <- fetch_samples(cohort = "NSCLC", data_synapse = out$cohort_ca_dx)
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#' out <- create_cohort(cohort = "NSCLC",
#'      regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin, Etoposide"),
#'      regimen_order = 1,
#'      regimen_order_type = "within regimen")
#' samples_data <- fetch_samples(cohort = "NSCLC", data_synapse = out$cohort_ca_dx)
#' @import
#' dplyr
#' dtplyr
#' tibble

fetch_samples <- function(cohort, data_synapse, df_record_ids) {
  if (missing(cohort)) {
    stop("You must provide a cohort name ('NSCLC', 'CRC', 'BrCa')
         in the `cohort` argument.")
  }

  cohort_temp <- cohort

  if (missing(df_record_ids)) {
    stop("You must provide an object that contains the variable record_id.")
  }

  if (min(grepl("record_id", names(df_record_ids))) == 1) {
    stop("You must provide an object that contains a variable called
         record_id.")
  }

  # keep records based on patient ID + cancer sequence of interest
  cohort_cpt <- dplyr::inner_join(df_record_ids %>%
    dplyr::select(.data$cohort, .data$record_id, .data$ca_seq),
  pluck(data_synapse, paste0("cpt_", cohort_temp)),
  by = c("cohort", "record_id", "ca_seq")
  ) %>%
    distinct() %>%
    mutate(sample_type = case_when(
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
      %in% c("6", "unspecified") ~ "Unspecified",
      str_to_lower(.data$cpt_sample_type)
      %in% c("7", "not applicable or hematologic malignancy") ~
        "Not applicable or hematologic malignancy"
    ))

  return(cohort_cpt)
}
