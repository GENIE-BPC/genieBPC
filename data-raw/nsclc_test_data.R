# Create fake example data set derived from NSCLC public
library(genieBPC)

genieBPC::set_synapse_credentials()

# pull NSCLC v2.0-public
nsclc_data <- genieBPC::pull_data_synapse("NSCLC", version = "v2.0-public")
nsclc_data <- nsclc_data$NSCLC_v2.0

# randomly sample from cancer diagnosis dataset, stratified by stage
set.seed(1123)

nsclc_ca_dx_sample <- nsclc_data$ca_dx_index %>%
  filter(stage_dx != "Stage I-III NOS") %>%
  group_by(stage_dx) %>%
  sample_n(50) %>%
  ungroup() %>%
  select(cohort, record_id_original = record_id) %>%
  # define test record ID
  mutate(record_id =
           paste0("GENIE-TEST-", str_pad(string = 1:n(), pad = 0, width = 4)))

# check that each patient is only sampled one time (affects merges below)
# nsclc_ca_dx_sample %>% count(record_id_original) %>% arrange(desc(n))

# get clinical data to those patients
nsclc_clinical_data <- nsclc_data %>%
  keep(!(names(.) %in% c("fusions", "cna", "mutations_extended")))

# subset clinical data for selected patients
nsclc_test_data_clinical <- purrr::map(nsclc_clinical_data, function(df){
  inner_join(nsclc_ca_dx_sample,
             df,
             by = c("cohort", "record_id_original" = "record_id"))
})

## subset genomic data for selected patients
# mutations
nsclc_test_data_mutations <- inner_join(nsclc_data$mutations_extended,
                                        nsclc_test_data_clinical$cpt,
                                        by = c("Tumor_Sample_Barcode" = "cpt_genie_sample_id")) %>%
  # replace original tumor sample barcode with new dummy ID
  rename(Tumor_Sample_Barcode_old = Tumor_Sample_Barcode) %>%
  mutate(Tumor_Sample_Barcode = str_replace(string = Tumor_Sample_Barcode_old,
                                           pattern = record_id_original,
                                           replacement = record_id)) %>%
  select(-Tumor_Sample_Barcode_old)

# fusions
nsclc_test_data_fusions <- inner_join(nsclc_data$fusions,
                                      nsclc_test_data_clinical$cpt,
                                      by = c("Tumor_Sample_Barcode" = "cpt_genie_sample_id")) %>%
  # replace original tumor sample barcode with new dummy ID
  rename(Tumor_Sample_Barcode_old = Tumor_Sample_Barcode) %>%
  mutate(Tumor_Sample_Barcode = str_replace(string = Tumor_Sample_Barcode_old,
                                            pattern = record_id_original,
                                            replacement = record_id)) %>%
  select(-Tumor_Sample_Barcode_old)

# CNA
nsclc_test_data_cna <- nsclc_data$cna %>%
  keep(names(.) %in% c("Hugo_Symbol",
                       str_replace_all(string = nsclc_test_data_clinical$cpt$cpt_genie_sample_id,
                                       pattern = "-", replace = "\\.")))

cna_xwalk <- left_join(tibble(tumor_sample_barcode = names(nsclc_test_data_cna)) %>%
                         filter(tumor_sample_barcode != "Hugo_Symbol") %>%
                         mutate(record_id_original = case_when(
                   grepl("DFCI|VICC|UHN", tumor_sample_barcode) ~
                     str_replace_all(word(tumor_sample_barcode, start = 1, end = 3, sep = "\\."),
                                     pattern = "\\.",
                                     replacement = "-")
                 ,
                 grepl("MSK", tumor_sample_barcode) ~
                   str_replace_all(word(tumor_sample_barcode, start = 1, end = 4, sep = "\\."),
                                   pattern = "\\.",
                                   replacement = "-")
                 # grepl("UHN", tumor_sample_barcode) ~
                 )),
          nsclc_ca_dx_sample %>% select(contains("record_id")),
          by = "record_id_original") %>%
  # concatenate tumor sample ID with record_id
  # CNA req 1 col/sample
  mutate(end_of_sample_id = case_when(
    grepl("DFCI|UHN", tumor_sample_barcode) ~ str_replace_all(
      word(tumor_sample_barcode, start = 4, end = 4, sep = "\\."),
      pattern = "\\.", replacement = "-") ,
    grepl("VICC", tumor_sample_barcode) ~ str_replace_all(
      word(tumor_sample_barcode, start = 4, end = 5, sep = "\\."),
      pattern = "\\.", replacement = "-") ,
    grepl("MSK", tumor_sample_barcode) ~
      str_replace_all(
        word(tumor_sample_barcode, start = 5, end = 6, sep = "\\."),
        pattern = "\\.", replacement = "-")),
    record_id_for_cna = paste0(record_id, "-", end_of_sample_id))

# rename cna data
names(nsclc_test_data_cna) <- c("Hugo_Symbol", cna_xwalk$record_id_for_cna)

# for cpt file, have to reformat cpt_genie_sample_id (do this below above reconfiguring)
nsclc_test_data_clinical$cpt <- nsclc_test_data_clinical$cpt %>%
  mutate(end_of_sample_id = case_when(
    grepl("DFCI|UHN", cpt_genie_sample_id) ~ str_replace_all(
      word(cpt_genie_sample_id, start = 4, end = 4, sep = "-"),
      pattern = "\\.", replacement = "-") ,
    grepl("VICC", cpt_genie_sample_id) ~ str_replace_all(
      word(cpt_genie_sample_id, start = 4, end = 5, sep = "-"),
      pattern = "\\.", replacement = "-") ,
    grepl("MSK", cpt_genie_sample_id) ~
      str_replace_all(
        word(cpt_genie_sample_id, start = 5, end = 6, sep = "-"),
        pattern = "\\.", replacement = "-")),
    cpt_genie_sample_id = paste0(record_id, "-", end_of_sample_id)) %>%
  select(-end_of_sample_id)

# remove record_id_original from clinical data (needed it temporarily to create dummy ID on genomic data)
nsclc_test_data_clinical <- purrr::map(nsclc_test_data_clinical, function(df){
  df %>%
    select(-record_id_original)
})

# combine clinical + genomic data together in a single list of the test data
nsclc_test_data <- c(nsclc_test_data_clinical,
                     list("mutations_extended" = nsclc_test_data_mutations,
                          "fusions" = nsclc_test_data_fusions,
                          "cna" = nsclc_test_data_cna))

names(nsclc_test_data)

usethis::use_data(nsclc_test_data, overwrite = TRUE)



