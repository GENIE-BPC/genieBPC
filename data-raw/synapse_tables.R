library(tidyverse)
library(synapser)
synapser::synLogin()

# manually record release dates (can't pull from Synapse)
release_dates <- tibble::tribble(
  ~cohort, ~version, ~release_date,
  "NSCLC", "v1.1-consortium", "2020-10",
  "NSCLC", "v2.1-consortium", "2021-08",
  "NSCLC", "v2.0-public", "2022-05",
  "CRC", "v1.1-consortium", "2021-02",
  "CRC", "v1.2-consortium", "2021-08",
  "BrCa", "v1.1-consortium", "2021-10",
  "CRC", "v2.0-public", "2022-10",
  "PANC", "v1.1-consortium", "2022-02",
  "BLADDER", "v1.1-consortium", "2022-11",
  "BrCa", "v1.2-consortium", "2022-10",
  "Prostate", "v1.1-consortium", "2022-03",
  "PANC", "v1.2-consortium", "2023-01",
  "Prostate", "v1.2-consortium", "2023-01"
)

# get list of all folders in the consortium and public data releases folders
data_release_folders <- list("syn21241322", "syn27056700")
names(data_release_folders) <- c("Consortium", "Public")

release_cohorts <- purrr::map_df(data_release_folders,
  ~ dplyr::bind_rows(as.list(synapser::synGetChildren(.))),
  .id = "release"
) %>%
  dplyr::select(release, name, id) %>%
  dplyr::filter(name != "Main GENIE cBioPortal Releases") %>%
  dplyr::mutate(release_cohort = paste0(release, "--", name)) %>%
  split(.$release_cohort) %>%
  purrr::map(., "id")

# for each cohort, get subfolders
releases_by_cohort <- purrr::map_df(release_cohorts,
  ~ dplyr::bind_rows(as.list(synGetChildren(.))),
  .id = "cohort"
) %>%
  dplyr::select(cohort, name, id) %>%
  dplyr::mutate(release_cohort_version = paste0(cohort, "--", name)) %>%
  split(., ~release_cohort_version)

# for each data release, get subfolders (clinical files, portal, documentation)
releases_by_cohort_subfolders <- purrr::map_df(releases_by_cohort,
  ~ dplyr::bind_rows(as.list(synGetChildren(.))),
  .id = "cohort_release"
) %>%
  dplyr::select(cohort_release, name, id) %>%
  dplyr::mutate(cohort_release_folder = paste0(cohort_release, "--", name)) %>%
  split(., ~cohort_release_folder) %>%
  purrr::map(., "id")

# get names of items in the folder
all_syn_ids <- purrr::map_df(releases_by_cohort_subfolders,
  ~ dplyr::bind_rows(as.list(synGetChildren(.))),
  .id = "cohort_release_folder"
) %>%
  dplyr::select(cohort_release_folder, name, id) %>%
  tidyr::separate(cohort_release_folder,
    into = c("release", "cohort", "version", "folder"),
    sep = "--", extra = "merge"
  ) %>%
  # exclude files that are the public data release that's published in the consortium folder
  dplyr::filter(!(grepl("public", version, ignore.case = TRUE) &
    release == "Consortium")) %>%
  # add "v" in front of version
  dplyr::mutate(version = paste0("v", version))

# keep files of interest
synapse_tables <- all_syn_ids %>%
  dplyr::filter((grepl("clinical_data", folder) & !grepl("DEC2019", name)) |
    (grepl("synopsis|fusions|CNA|mutations_extended", name) &
      !grepl("meta", name))) %>%
  dplyr::mutate(
    filename_pre = str_remove(
      pattern = ".csv|.txt",
      string = name
    ),
    filename = case_when(
      grepl("synopsis", filename_pre) ~ "variable_synopsis",
      TRUE ~ filename_pre
    )
  ) %>%
  dplyr::mutate(df = factor(
    case_when(
      str_to_lower(filename) == "patient_level_dataset" ~ "pt_char",
      str_to_lower(filename) == "cancer_level_dataset_index" ~ "ca_dx_index",
      str_to_lower(filename) == "cancer_level_dataset_non_index" ~ "ca_dx_non_index",
      str_to_lower(filename) == "regimen_cancer_level_dataset" ~ "ca_drugs",
      str_to_lower(filename) == "imaging_level_dataset" ~ "prissmm_imaging",
      str_to_lower(filename) == "pathology_report_level_dataset" ~ "prissmm_pathology",
      str_to_lower(filename) == "ca_radtx_dataset" ~ "ca_radtx",
      str_to_lower(filename) == "med_onc_note_level_dataset" ~ "prissmm_md",
      str_to_lower(filename) == "tm_level_dataset" ~ "tumor_marker",
      str_to_lower(filename) == "cancer_panel_test_level_dataset" ~ "cpt",
      str_to_lower(filename) == "data_cna" ~ "cna",
      str_to_lower(filename) == "data_fusions" ~ "fusions",
      str_to_lower(filename) == "data_mutations_extended" ~ "mutations_extended",
      TRUE ~ filename
    ),
    levels = c(
      "pt_char", "ca_dx_index", "ca_dx_non_index",
      "ca_drugs", "prissmm_imaging", "prissmm_pathology",
      "ca_radtx", "prissmm_md", "tumor_marker",
      "cpt", "mutations_extended", "fusions", "cna",
      "variable_synopsis"
    )
  )) %>%
  # merge on release dates
  dplyr::left_join(., release_dates,
    by = c("cohort", "version")
  ) %>%
  dplyr::select(release_date, cohort, version, df, synapse_id = id) %>%
  dplyr::arrange(release_date, cohort, version, df)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
