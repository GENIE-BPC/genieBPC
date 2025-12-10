test_that("expected datasets", {
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
                 "mutations_extended", "fusions", "sv", "cna",
                 "variable_synopsis"))

  # expect all data releases to have the following data frames
  # (no longer expect all to have fusions)
  expect_equal(nrow(synapse_tables_wide),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(pt_char, ca_dx_index, ca_dx_non_index,
                              ca_drugs, prissmm_imaging, prissmm_pathology,
                              prissmm_md, cpt,
                              mutations_extended, cna,
                              variable_synopsis)))

  # expect only certain cohorts to have tumor marker data
  synapse_tables_wide_tm_cohorts <- synapse_tables_wide %>%
    dplyr::filter(stringr::str_to_upper(cohort) %in% c("CRC", "BRCA", "PANC", "PROSTATE"))

  expect_equal(nrow(synapse_tables_wide_tm_cohorts),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(tumor_marker)))

  # expect only certain cohorts to have radiation data
  synapse_tables_wide_rt_cohorts <- synapse_tables_wide %>%
    dplyr::filter(stringr::str_to_upper(cohort) %in% c("PANC", "PROSTATE", "BLADDER", "RENAL") |
                    (stringr::str_to_upper(cohort) == "NSCLC" &
                       !(version %in% c("v1.1-consortium",
                                        "v2.2-consortium",
                                        "v2.0-public"))) |
                    (stringr::str_to_upper(cohort) == "CRC" &
                       !(version %in% c("v1.3-consortium",
                                        "v2.0-public"))))

  expect_equal(nrow(synapse_tables_wide_rt_cohorts),
               nrow(synapse_tables_wide %>%
                      tidyr:: drop_na(ca_radtx)))

  # expect only certain cohorts + data releases to have fusions data
  synapse_tables_wide_fusions_releases <- synapse_tables_wide %>%
    dplyr::filter(as.Date(paste0(release_date, "-01"), format = "%Y-%m-%d") <
                    as.Date("2024-04-01", "%Y-%m-%d"))

  expect_equal(nrow(synapse_tables_wide_fusions_releases),
               nrow(synapse_tables_wide %>%
                      tidyr:: drop_na(fusions)))

  # expect only certain cohorts + data releases to have SV data
  synapse_tables_wide_sv_releases <- synapse_tables_wide %>%
    dplyr::filter(as.Date(paste0(release_date, "-01"), format = "%Y-%m-%d") >=
                    as.Date("2023-11-01", "%Y-%m-%d") |
                  (stringr::str_to_upper(cohort) == "PANC" &
                    version == "v1.2-consortium"))

  expect_equal(nrow(synapse_tables_wide_sv_releases),
               nrow(synapse_tables_wide %>%
                      tidyr:: drop_na(sv)))

  # cohort, version and release dates always populated"
  expect_equal(nrow(synapse_tables_wide),
               nrow(synapse_tables_wide %>%
                      tidyr::drop_na(cohort, version, release_date)))
})
