#' treatment_history
#'
#' This function allows the user to get the complete treatment course of a list of patients.
#' @param record_ids character vector of the patient IDs of interest.
#' @param ca_drugs dataset `ca_drugs` from `pull_data_synapse()` function.
#' @param lines_keep regimen number to be kept to create the summary.
#' @return Returns data frame `treat_hist` and interactive plot `p_dist`.
#' @export
#'
#' @examples
#' # pull_data_synapse("NSCLC")
#' # record_ids <- ca_dx_index_NSCLC$record_id[ca_dx_index_NSCLC$stage_dx == "Stage IV"]
#' # ca_drugs <- ca_drugs_NSCLC
#' # lines_keep = 1:3
#' # treatment_history(record_ids = record_ids, ca_drugs = ca_drugs,lines_keep = lines_keep)
#' @import
#' dplyr
#' TraMineR
#' sunburstR
#' pipeR


treatment_history <- function(record_ids, ca_drugs,lines_keep = NULL){

  if(is.null(lines_keep))
    lines_keep = min(ca_drugs$regimen_number, na.rm = T) : max(ca_drugs$regimen_number, na.rm = T)

  dat <- ca_drugs %>%
    filter(record_id %in% record_ids,
           regimen_number %in% lines_keep)

  temp_dat <- dat %>%
    select(record_id, regimen_number,regimen_drugs)

  temp_temp_dat <- temp_dat %>%
    mutate(regimen_number = paste0("R",regimen_number)) %>%
    pivot_wider(names_from = regimen_number,
                values_from = regimen_drugs) %>%
    select(record_id, starts_with("R")) %>%
    rowwise() %>%
    mutate_at(vars(matches("R")), ~ ifelse(is.null(.),"",.)) %>%
    ungroup()

  path <- c()
  for(i in 1:nrow(temp_temp_dat)){
    temp_path <- as.character(unlist(temp_temp_dat[i,grep("R",colnames(temp_temp_dat))]))
    path[i] <- paste0(temp_path[temp_path != ""],collapse = "-")
  }

  temp_temp_dat$path <- path
  temp_temp_dat <- temp_temp_dat %>%
  select(record_id, path)


  test_dat <- temp_temp_dat %>%
    group_by(path) %>%
    summarise(Pop = length(unique(record_id))) %>%
    ungroup()

  p <- sunburst(test_dat, legend=TRUE)

  return(list("treat_hist" = temp_temp_dat,"p_dist" = p))
}
