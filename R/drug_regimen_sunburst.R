#' drug_regimen_sunburst
#'
#' This function allows the user to visualize the complete treatment course for selected diagnoses.
#' @param ids dataframe with columns `record_id` and `ca_seq` from the downloaded data.
#' @param ca_drugs dataset `ca_drugs` from `pull_data_synapse()` function.
#' @param lines_keep regimen number to be kept to create the summary.
#' @return Returns data frame `treatment_history` and interactive plot `sunburst_plot`.
#' @export
#'
#' @examples
#' # nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
#' # record_ids <- nsclc_data$ca_dx_index_NSCLC
#' # ca_drugs <- nsclc_data$ca_drugs_NSCLC
#' # regimen_drugs <- unique(ca_drugs$regimen_drugs)
#' # lines_keep = 1:3
#' # test1 <- drug_regimen_sunburst(ids = record_ids, ca_drugs = ca_drugs,
#' #                            regimen_drugs = regimen_drugs, lines_keep = lines_keep)
#' @import
#' dplyr
#' TraMineR
#' sunburstR
#' pipeR
#' tidyr


drug_regimen_sunburst <- function(data_synapse,
                                  data_cohort,
                                  lines_keep = NULL) {

  # get the name of the cohort from the data_synapse object naming convention
  cohort_temp <- word(names(data_synapse)[1], 3, sep = "_")

  # if no lines of therapy are specified, select all lines of therapy
  if (is.null(lines_keep)) {
    # get range of all lines of therapy
    lines_keep <- min(pluck(data_synapse,
                            paste0("ca_drugs", cohort_temp))$regimen_number,
                      na.rm = T):max(pluck(data_synapse,
                                           paste0("ca_drugs", cohort_temp))$regimen_number,
                                     na.rm = T)
  }

  # if (is.null(first_regimen)) {
  #   first_regimen <- pluck(data_cohort, "cohort_ca_drugs")$regimen_drugs
  # }

  # get all regimens to diagnoses in cohort
  cohort_all_drugs <- left_join(pluck(data_cohort, "cohort_ca_dx"),
                   pluck(data_synapse, paste0("ca_drugs_", cohort_temp)),
                   by = c("cohort", "record_id", "ca_seq", "institution"))

  # subset on regimen numbers of interest, if applicable
  # (selects all lines if lines_keep is left blank, i.e. doesn't subset at all)
  cohort_reg_nums_of_interest <- cohort_all_drugs %>%
    filter(regimen_number %in% lines_keep)

  # prepare the data for the sunburst function
  # 1 column per regimen (R1, R2, R3, etc.)
  cohort_for_sunburst <- cohort_reg_nums_of_interest %>%
    select(record_id, regimen_number, regimen_drugs) %>%
    mutate(regimen_number = paste0("R", regimen_number)) %>%
    pivot_wider(
      names_from = regimen_number,
      values_from = regimen_drugs
    ) %>%
    select(record_id, starts_with("R")) %>%
    mutate_at(vars(matches("R")), ~ as.character(.)) %>%
    rowwise() %>%
    mutate_at(
      # vars(matches("R")), ~ ifelse(. == "NULL","",.),
      vars(matches("R")), ~ ifelse(is.na(.) || . == "NULL", "", .)
    ) %>%
    ungroup()

  # set up drug regimen data for sunburst
  # concatenate drug regimens separated by "-" into variable path
  path <- c()
  for (i in 1:nrow(cohort_for_sunburst)) {
    temp_path <- as.character(unlist(cohort_for_sunburst[i, grep("R", colnames(cohort_for_sunburst))]))
    path[i] <- paste0(temp_path[temp_path != ""], collapse = "-")
  }

  # add path onto dataset
  cohort_for_sunburst$path <- path

  # only keep each record ID plus drug regimen sequence (1 row/patient)
  cohort_for_sunburst <- cohort_for_sunburst %>%
    select(record_id, path)

  # summarize number of patients with each regimen combination
  # 1 row per regimen combination with number of patients that got that regimen sequence
  df_for_sunburst <- cohort_for_sunburst %>%
    group_by(path) %>%
    summarise(Pop = length(unique(record_id))) %>%
    ungroup()

  # create the sunburst plot
  sunburst_plot <- sunburst(df_for_sunburst, legend = TRUE)

  # return treatment data and sunburst plot
  return(list("treatment_history" = df_for_sunburst,
              "sunburst_plot" = sunburst_plot
              ))
}
