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
               values_to = "drug_name",
               values_drop_na = TRUE) %>%
  select(-drug_no) %>%
  mutate(drug_name_short = word(drug_name, sep = "\\(")) %>%
  arrange(cohort, drug_name_short) %>%
  distinct() %>%
  select(cohort, drug_name_short, drug_name) %>%
  filter(cohort %in% c("NSCLC", "CRC", "BrCa")) %>%
  split(.$cohort)


for(i in 1:length(drug_names_by_cohort)){
  file_name = paste0(str_to_lower(names(drug_names_by_cohort)[[i]]), "_cohort_drug_names")
  save(drug_names_by_cohort,
       file = here::here(paste0("data/", file_name, ".RData")))
}

