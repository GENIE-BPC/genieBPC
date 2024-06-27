#' Visualize drug regimen sequences in a sunburst plot
#'
#' This function allows the user to visualize the complete treatment
#' course for selected cancer diagnoses.
#'
#' See the \href{https://genie-bpc.github.io/genieBPC/articles/drug_regimen_sunburst_vignette.html}{drug_regimen_sunburst vignette}
#' for additional details and examples.
#'
#' @param data_synapse The item from the nested list returned from
#' `pull_data_synapse()`
#' @param data_cohort The list returned from the `create_analytic_cohort()`
#' function call
#' @param max_n_regimens The maximum number of regimens displayed in the
#'   sunburst plot
#' @param ... Additional parameters passed to `sunburstR::sunburst()`
#' @return Returns data frame `treatment_history` and interactive
#' plot `sunburst_plot`
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' # Example using package test data
#' # get clinico-genomic files for a specific cohort
#' nsclc_sub <- create_analytic_cohort(
#'   data_synapse = genieBPC::nsclc_test_data,
#'   stage_dx = c("Stage III", "Stage IV")
#' )
#'
#' # create sunburst plot
#' ex1 <- drug_regimen_sunburst(
#'   data_synapse = nsclc_test_data,
#'   data_cohort = nsclc_sub,
#'   max_n_regimens = 3
#' )
#'
#' @examplesIf genieBPC::.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))
#' # Example 2 ----------------------------------
#' # using pull_data_synapse
#' nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#'
#' nsclc_stg_iv <- create_analytic_cohort(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   stage = "Stage IV"
#' )
#'
#' ex2 <- drug_regimen_sunburst(
#'   data_synapse = nsclc_2_0$NSCLC_v2.0,
#'   data_cohort = nsclc_stg_iv,
#'   max_n_regimens = 3
#' )
#'
#' @import
#' dplyr
#' sunburstR
#' tidyr


drug_regimen_sunburst <- function(data_synapse,
                                  data_cohort,
                                  max_n_regimens = NULL,
                                  ...) {

  # make sure all required parameters are specified
  if (missing(data_synapse) | !is.list(data_synapse)) {
    stop("Specify the list object returned from pull_data_synapse in the
         `data_synapse` parameter.")
  }

  if (missing(data_cohort) | !is.list(data_cohort)) {
    stop("Specify the list object returned from create_analytic_cohort in the
         `data_cohort` parameter.")
  }

  # if the data_synapse parameter is a list but not the right list
  if (is.null(names(data_synapse))) {
    stop("Specify the list object returned from pull_data_synapse in the
         `data_synapse` parameter.")
  } else if (sum(grepl(
    "pt_char|ca_dx|ca_drugs|prissmm|cpt|cna|fusions|mutations",
    names(data_synapse)
  )) < 11) {
    stop("Specify the list object returned from pull_data_synapse in the
         `data_synapse` parameter containing the nested list with the dataframes:
         pt_char, ca_dx_index, ca_dx_non_index, ca_drugs,
         prissmm_imaging, prissmm_pathology, prissmm_md, cpt,
         mutations_extended, fusions, and cna.")
  }


  # get the name of the cohort from the data_synapse object naming convention
  cohort_temp <- word(names(data_synapse)[1], 3, sep = "_")

  # if the data_cohort parameter is a list but not the right list
  # checking the names of the list inputs
  if (is.null(data_cohort$cohort_pt_char) |
    is.null(data_cohort$cohort_ca_dx) |
    is.null(data_cohort$cohort_ngs)) {
    stop("Specify the list object returned from create_analytic_cohort in the
         `data_cohort` parameter")
  }

  # if (class(max_n_regimens) != "numeric") {
  #   stop("Specify the maximum number of regimens to display.")
  # }

  # get all regimens to diagnoses in cohort
  cohort_all_drugs <- dplyr::inner_join(purrr::pluck(
    data_cohort,
    "cohort_ca_dx"
  ),
  purrr::pluck(data_synapse, "ca_drugs"),
  by = c("cohort", "record_id", "ca_seq", "institution")
  ) %>%
    # create drug regimen order WITHIN the cancer diagnosis
    # (variable regimen_number is the order of the regimen w/in the patient)
    dplyr::group_by(.data$cohort, .data$record_id, .data$ca_seq) %>%
    dplyr::arrange(
      .data$cohort, .data$record_id,
      .data$ca_seq, .data$regimen_number
    ) %>%
    dplyr::mutate(order_within_cancer = 1:n()) %>%
    dplyr::ungroup()

  # if no lines of therapy are specified, select all lines of therapy
  if (is.null(max_n_regimens)) {
    # get range of all lines of therapy
    max_n_regimens <- max(cohort_all_drugs$order_within_cancer,
      na.rm = TRUE
    )
  }

  # subset on regimen numbers of interest, if applicable
  # (selects all lines if max_n_regimens is left blank,
  # i.e. doesn't subset at all)
  cohort_reg_nums_of_interest <- cohort_all_drugs %>%
    filter(.data$order_within_cancer %in% 1:max_n_regimens)

  # prepare the data for the sunburst function
  # 1 column per regimen (R1, R2, R3, etc.)
  cohort_for_sunburst <- cohort_reg_nums_of_interest %>%
    dplyr::select(
      "record_id",
      "order_within_cancer",
      "regimen_drugs"
    ) %>%
    dplyr::mutate(
      order_within_cancer =
        paste0("R", .data$order_within_cancer)
    ) %>%
    tidyr::pivot_wider(
      names_from = "order_within_cancer",
      values_from = "regimen_drugs"
    ) %>%
    dplyr::select("record_id", starts_with("R")) %>%
    dplyr::mutate_at(vars(matches("R")), ~ as.character(.)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_at(
      # vars(matches("R")), ~ ifelse(. == "NULL","",.),
      vars(matches("R")), ~ ifelse(is.na(.) || . == "NULL", "", .)
    ) %>%
    dplyr::ungroup()

  # set up drug regimen data for sunburst
  # concatenate drug regimens separated by "-" into variable path
  path <- c()
  for (i in seq_len(nrow(cohort_for_sunburst))) {
    temp_path <- as.character(
      unlist(cohort_for_sunburst[i, grep("R", colnames(cohort_for_sunburst))])
    )
    path[i] <- paste0(temp_path[temp_path != ""], collapse = "-")
  }

  # add path onto dataset
  cohort_for_sunburst$path <- path

  # only keep each record ID plus drug regimen sequence (1 row/patient)
  cohort_for_sunburst <- cohort_for_sunburst %>%
    dplyr::select("record_id", "path")

  # summarize number of patients with each regimen combination
  # 1 row per regimen combination with number of patients that
  # got that regimen sequence
  df_for_sunburst <- cohort_for_sunburst %>%
    dplyr::group_by(.data$path) %>%
    dplyr::summarise(Pop = length(unique(.data$record_id))) %>%
    dplyr::ungroup()

  # create the sunburst plot
  sunburst_plot <- sunburst(df_for_sunburst, legend = TRUE, ...)

  # return treatment data and sunburst plot
  return(list(
    "treatment_history" = df_for_sunburst,
    "sunburst_plot" = sunburst_plot
  ))
}
