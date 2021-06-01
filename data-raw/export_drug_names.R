#' Drug Names by Cohort
#'
#' A dataset containing the cancer-directed drug names and their synonyms.
#'
#' @format A table for cancer-directed drug names associated with each cancer cohort:
#' \describe{
#'   \item{cohort}{GENIE BPC Project cancer. Must be one of "NSCLC" (non-small cell lung cancer) or "CRC" (colorectal cancer). Future cohorts will include "BrCa" (breast cancer), "PANC" (pancreatic cancer), "Prostate" (prostate cancer).}
#'   \item{drug_name}{Name of generic/ingredient cancer-directed drug}
#'   \item{drug_name_full}{Name of generic/ingredient cancer-directed drug with associated synonyms in parentheses}
#'   ...
#' }

library(tidyverse)
library(synapser)

synLogin()

# load current version of derived data from Synapse
derived_data_file_synapse <- synGet("syn22299362")
load(derived_data_file_synapse$path)

drug_names_by_cohort =
  inner_join(ca_dx_derived_index_redacted,
             ca_drugs_derived_redacted,
             by = c("cohort", "record_id", "institution", "ca_seq")) %>%
  select(cohort, starts_with("drugs_drug")) %>%
  pivot_longer(cols = starts_with("drugs_drug"),
               names_to = "drug_no",
               values_to = "drug_name_full",
               values_drop_na = TRUE) %>%
  select(-drug_no) %>%
  mutate(drug_name = word(drug_name_full, sep = "\\(")) %>%
  arrange(cohort, drug_name) %>%
  distinct() %>%
  select(cohort, drug_name, drug_name_full) %>%
  filter(cohort %in% c("NSCLC", "CRC"))

attr(drug_names_by_cohort$drug_name, "label") <- "Drug Name"
attr(drug_names_by_cohort$drug_name_full, "label") <- "Full Drug Name"

usethis::use_data(drug_names_by_cohort, overwrite = TRUE)
