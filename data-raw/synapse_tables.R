synapse_tables <- readxl::read_excel("data-raw/synapse_tables.xlsx") %>%
  # TEMPORARY FIX TO KEEP FUNCTIONS WORKING,
  # WILL REMOVE
  mutate(dataset = case_when(
    dataset == "patient_level_dataset" ~ "pt_char",
    dataset == "cancer_level_dataset_index" ~ "ca_dx_index",
    dataset == "cancer_level_dataset_non_index" ~ "ca_dx_non_index",
    dataset == "regimen_cancer_level_dataset" ~ "ca_drugs",
    dataset == "imaging_level_dataset" ~ "prissmm_imaging",
    dataset == "pathology_report_level_dataset" ~ "prissmm_pathology",
    dataset == "ca_radtx_dataset" ~ "ca_radtx",
    dataset == "med_onc_note_level_dataset" ~ "prissmm_md",
    dataset == "tm_level_dataset" ~ "tumor_marker",
    dataset == "cancer_panel_test_level_dataset" ~ "cpt",
    dataset == "data_cna" ~ "cna",
    dataset == "data_fusions" ~ "fusions",
    dataset == "data_mutations_extended" ~ "mutations_extended"
  )) %>%
  rename(df = dataset)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)

