#' drug_regimen_sunburst
#'
#' This function allows the user to visualize the complete treatment course for selected diagnoses.
#' @param data_synapse The list returned from the `pull_data_synapse()` function call
#' @param data_cohort The list returned from the `create_analytic_cohort()` function call
#' @param max_n_regimens The number of regimens displayed in the sunburst plot
#' @return Returns data frame `treatment_history` and interactive plot `sunburst_plot`
#' @export
#'
#' @examples
#' # nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
#' # nsclc_cohort <- create_analytic_cohort(cohort = "NSCLC", data_synapse = "nsclc_data", stage = "Stage IV")
#' # ex1 <- drug_regimen_sunburst(data_synapse = nsclc_data, data_cohort = nsclc_cohort,
#' max_n_regimens = 3)
#' @import
#' dplyr
#' TraMineR
#' sunburstR
#' pipeR
#' tidyr


drug_regimen_sunburst <- function(data_synapse,
                                  data_cohort,
                                  max_n_regimens) {

  # get the name of the cohort from the data_synapse object naming convention
  cohort_temp <- word(names(data_synapse)[1], 3, sep = "_")

  # make sure all required parameters are specified
  if (missing(data_synapse) | class(data_synapse) != "list") {
    stop("Specify the list object returned from pull_data_synapse in the
         `data_synapse` parameter.")
  }

  # if the data_synapse parameter is a list but not the right list
  if (is.null(names(data_synapse)) |
      min(grepl("pt_char|ca_dx|ca_drugs|prissmm|cpt|cna|fusions|mutations",
                names(data_synapse))) == 0){
    stop("Specify the list object returned from pull_data_synapse in the
         `data_synapse` parameter.")
  }

  if (missing(data_cohort) | class(data_cohort) != "list") {
    stop("Specify the list object returned from create_analytic_cohort in the
         `data_cohort` parameter.")
  }

  # if the data_cohort parameter is a list but not the right list
  # checking the names of the list inputs
  if (is.null(names(data_cohort)) |
      min(grepl("cohort_ca_dx|cohort_ca_drugs|cohort_ngs", names(data_cohort)[1:3]))
      == 0){
    stop("Specify the list object returned from create_analytic_cohort in the
         `data_cohort` parameter")
  }

  if (class(max_n_regimens) != "numeric"){
    stop("Specify the maximum number of regimens to display.")
  }

  # if no lines of therapy are specified, select all lines of therapy
  if (is.null(max_n_regimens)) {
    # get range of all lines of therapy
    max_n_regimens <- 1:max(pluck(
      data_synapse,
      paste0("ca_drugs", cohort_temp)
    )$regimen_number,
    na.rm = T
    )
  }

  # get all regimens to diagnoses in cohort
  cohort_all_drugs <- left_join(pluck(data_cohort, "cohort_ca_dx"),
    pluck(data_synapse, paste0("ca_drugs_", cohort_temp)),
    by = c("cohort", "record_id", "ca_seq", "institution")
  )

  # subset on regimen numbers of interest, if applicable
  # (selects all lines if max_n_regimens is left blank, i.e. doesn't subset at all)
  cohort_reg_nums_of_interest <- cohort_all_drugs %>%
    filter(regimen_number %in% 1:max_n_regimens)

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
  return(list(
    "treatment_history" = df_for_sunburst,
    "sunburst_plot" = sunburst_plot
  ))
}
