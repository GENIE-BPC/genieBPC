library(tidyverse)
library(genieBPC)

set_synapse_credentials()

# load current version of derived data from Synapse
data_pull <- pull_data_synapse(cohort = c("NSCLC", "CRC", "BrCa", "BLADDER", "PANC", "Prostate"),
                               version = c("v2.0-public", "v2.0-public", "v1.2-consortium",
                                           "v1.1-consortium", "v1.2-consortium", "v1.2-consortium"))

drug_regimen_list <-
  bind_rows(data_pull$NSCLC_v2.0$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))),
            data_pull$CRC_v2.0$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))),
            data_pull$BrCa_v1.2$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))),
            data_pull$PANC_v1.2$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))),
            data_pull$BLADDER_v1.1$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))),
            data_pull$Prostate_v1.2$ca_drugs %>% select(c(cohort, starts_with("drugs_drug"))))%>%
  select(-contains("oth")) %>%
  pivot_longer(
    cols = !cohort,
    names_to = "drug_no",
    values_to = "drug_name_full",
    values_drop_na = TRUE
  ) %>%
  select(-drug_no) %>%
  mutate(drug_name = word(drug_name_full, sep = "\\(")) %>%
  arrange(cohort, drug_name) %>%
  distinct() %>%
  filter(drug_name != "") %>%
  select(cohort, drug_name, drug_name_full)

attr(drug_regimen_list$drug_name, "label") <- "Drug Name"
attr(drug_regimen_list$drug_name_full, "label") <- "Full Drug Name"

usethis::use_data(drug_regimen_list, overwrite = TRUE)
