test_that("expected datasts", {
  # transpose to 1 col/dataframe
  synapse_tables_wide <- genieBPC::synapse_tables %>%
    dplyr::arrange(df) %>%
    tidyr::pivot_wider(id_cols = c(cohort, version, release_date),
                names_from = df,
                values_from = synapse_id,
    )

  # expect only pre-specified names
  expect_equal(names(synapse_tables_wide),
               c("cohort", "version", "release_date",
                 "pt_char", "ca_dx_index", "ca_dx_non_index",
                 "ca_drugs", "prissmm_imaging", "prissmm_pathology",
                 "ca_radtx", "prissmm_md", "tumor_marker", "cpt",
                 "mutations_extended", "fusions", "cna",
                 "variable_synopsis"))

  # expect all data releases to have the following data frames
  expect_equal(nrow(synapse_tables_wide),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(pt_char, ca_dx_index, ca_dx_non_index,
                              ca_drugs, prissmm_imaging, prissmm_pathology,
                              prissmm_md, cpt,
                              mutations_extended, fusions, cna,
                              variable_synopsis)))

  # expect only certain cohorts to have tumor marker data
  synapse_tables_wide_tm_cohorts <- synapse_tables_wide %>%
    dplyr::filter(stringr::str_to_upper(cohort) %in% c("CRC", "BRCA", "PANC", "PROSTATE"))

  expect_equal(nrow(synapse_tables_wide_tm_cohorts),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(tumor_marker)))

  # expect onlycertain cohorts to have radiation data
  synapse_tables_wide_rt_cohorts <- synapse_tables_wide %>%
    dplyr::filter(stringr::str_to_upper(cohort) %in% c("PANC", "PROSTATE", "BLADDER"))

  expect_equal(nrow(synapse_tables_wide_rt_cohorts),
               nrow(synapse_tables_wide %>%
                      tidyr:: drop_na(ca_radtx)))

  # cohort, version and release dates always populated"
  expect_equal(nrow(synapse_tables_wide),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(cohort, version, release_date)))
})
