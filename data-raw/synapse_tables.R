synapse_tables <- readxl::read_excel("data-raw/synapse_tables.xlsx") %>%
  # TEMPORARY FIX TO KEEP FUNCTIONS WORKING,
  # WILL REMOVE
  dplyr::mutate(dataset = factor(case_when(
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
  ),
  levels = c("pt_char", "ca_dx_index", "ca_dx_non_index",
             "ca_drugs", "prissmm_imaging", "prissmm_pathology",
             "ca_radtx", "prissmm_md", "tumor_marker",
             "cpt", "mutations_extended", "fusions", "cna"))) %>%
  dplyr::arrange(release_date, cohort, version, dataset) %>%
  dplyr::rename(df = dataset)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)

