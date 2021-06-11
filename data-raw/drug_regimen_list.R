library(tidyverse)
library(synapser)

synLogin()

# load current version of derived data from Synapse
derived_data_file_synapse <- synGet("syn22299362")
load(derived_data_file_synapse$path)

drug_regimen_list =
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
  filter(cohort %in% c("NSCLC", "CRC", "BrCa")) %>%
  mutate(drug_class = case_when(
    drug_name %in% c("Atezolizumab", "Durvalumab", "Ipilimumab", "Nivolumab",
                     "Pembrolizumab", "Tremelimumab", "Trastuzumab") ~ "Immunotherapy"))

attr(drug_regimen_list$drug_name, "label") <- "Drug Name"
attr(drug_regimen_list$drug_name_full, "label") <- "Full Drug Name"
attr(drug_regimen_list$drug_class, "label") <- "Drug Class"

usethis::use_data(drug_regimen_list, overwrite = TRUE)
