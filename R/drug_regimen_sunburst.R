#' drug_regimen_sunburst
#'
#' This function allows the user to visualize the complete treatment course for selected diagnoses.
#' @param ids dataframe with columns `record_id` and `ca_seq` from the downloaded data.
#' @param ca_drugs dataset `ca_drugs` from `pull_data_synapse()` function.
#' @param regimen_drugs Vector with names of drugs in cancer-directed regimen,
#' separated by a comma. For example, to specify a regimen consisting of
#' Carboplatin and Pemetrexed, specify regimen_drugs = "Carboplatin,
#' Pemetrexed". Acceptable values are found in the `drug_names_by_cohort`
#' dataset provided with this package.
#' @param lines_keep regimen number to be kept to create the summary.
#' @return Returns data frame `treat_hist` and interactive plot `sunburst_plot`.
#' @export
#'
#' @examples
#' # nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
#' # record_ids <- nsclc_data$ca_dx_index_NSCLC
#' # ca_drugs <- nsclc_data$ca_drugs_NSCLC
#' # regimen_drugs <- unique(ca_drugs$regimen_drugs)
#' # lines_keep = 1:3
#' # test1 <- treatment_history(ids = record_ids, ca_drugs = ca_drugs,
#' #                            regimen_drugs = regimen_drugs, lines_keep = lines_keep)
#' @import
#' dplyr
#' TraMineR
#' sunburstR
#' pipeR
#' tidyr


drug_regimen_sunburst <- function(ids, ca_drugs, regimen_drugs, lines_keep = NULL) {

  #
  if (is.null(lines_keep)) {
    lines_keep <- min(ca_drugs$regimen_number, na.rm = T):max(ca_drugs$regimen_number, na.rm = T)
  }

  dat <- data.frame()
  for (i in 1:nrow(ids)) {
    temp <- ca_drugs %>%
      filter(
        record_id %in% ids$record_id[i],
        ca_seq %in% ids$ca_seq[i]
      ) %>%
      arrange(regimen_number)

    if (length(which(temp$regimen_drugs %in% regimen_drugs)) > 0) {
      temp <- temp[which(temp$regimen_drugs %in% regimen_drugs)[1]:nrow(temp), ] %>%
        mutate(regimen_number = regimen_number - min(regimen_number) + 1)

      dat <- rbind(dat, temp)
    }
  }

  dat <- dat %>%
    filter(regimen_number %in% lines_keep)

  temp_dat <- dat %>%
    select(record_id, regimen_number, regimen_drugs)

  temp_temp_dat <- temp_dat %>%
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

  path <- c()
  for (i in 1:nrow(temp_temp_dat)) {
    temp_path <- as.character(unlist(temp_temp_dat[i, grep("R", colnames(temp_temp_dat))]))
    path[i] <- paste0(temp_path[temp_path != ""], collapse = "-")
  }

  temp_temp_dat$path <- path
  temp_temp_dat <- temp_temp_dat %>%
    select(record_id, path)


  test_dat <- temp_temp_dat %>%
    group_by(path) %>%
    summarise(Pop = length(unique(record_id))) %>%
    ungroup()

  p <- sunburst(test_dat, legend = TRUE)

  return(list("treat_hist" = temp_temp_dat, "sunburst_plot" = p))
}
