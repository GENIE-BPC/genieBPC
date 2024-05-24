library(tidyverse)
library(genieBPC)

set_synapse_credentials()

# load current version of derived data from Synapse for all cohorts and
# data releases
data_pull <- pull_data_synapse(cohort = synapse_version()$cohort,
                               version = synapse_version()$version)

# set all together, restructure to be 1 row per drug name, per data release
drug_regimen_list <- map(data_pull, pluck, "ca_drugs") %>%
  # sometimes release version is character, and sometimes it is numeric
  # drop release_version (can't easily update all to be character since
  # we don't have the release_version variable on earlier data releases)
  map_df(., select, -any_of("release_version"), .id = "cohort_data_release") %>%
  select(cohort_data_release, cohort, starts_with("drugs_drug")) %>%
  select(-contains("oth")) %>%
  pivot_longer(
    cols = c(-cohort, -cohort_data_release),
    names_to = "drug_no",
    values_to = "drug_name_full",
    values_drop_na = TRUE
  ) %>%
  select(-drug_no) %>%
  mutate(drug_name = word(drug_name_full, sep = "\\(")) %>%
  arrange(cohort_data_release, drug_name) %>%
  distinct() %>%
  filter(drug_name != "") %>%
  select(cohort, cohort_data_release, drug_name, drug_name_full)

attr(drug_regimen_list$drug_name, "label") <- "Drug Name"
attr(drug_regimen_list$drug_name_full, "label") <- "Full Drug Name"

usethis::use_data(drug_regimen_list, overwrite = TRUE)
