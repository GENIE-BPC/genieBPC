if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {
  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))

  # indicator for on CRAN or GitHub Actions
  on_CRAN_or_GH <- (testthat:::on_cran() | testthat:::on_ci())

  # data frame of each release to use for pmap
  # only pull most recent if on CRAN or GH Actions
  # if running locally, pull all data releases
  data_releases <- inner_join(
    genieBPC::synapse_version(most_recent = TRUE) %>% #on_CRAN_or_GH
      distinct(cohort, version),
    count(genieBPC::data_releases_expected_size, cohort, version,
      name = "expected_n_dfs"
    ),
    by = c("cohort", "version")
  ) %>%
    mutate(expected_n_dfs_with_summary = expected_n_dfs + 4)

  # for each data release, pull data into the R environment
  test_list <- pmap(
    data_releases %>%
      select(cohort, version),
    pull_data_synapse
  )

  # name the items in the list
  names(test_list) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
  )

  # get actual length of each data release returned from pull_data_synapse
  actual_length <- map_depth(test_list, .depth = 2, length) %>%
    bind_rows() %>%
    pivot_longer(
      cols = everything(),
      names_to = "data_release",
      values_to = "length",
      values_drop_na = TRUE
    )

  # for create_analytic_cohort tests
  data_releases_pull_data <- test_list %>%
    # remove blank level from list
    list_flatten() %>%
    # change variable types to align with create_analytic_cohort updates
    # that were required to stack data for multiple cohorts together
    map_depth(., 2, ~ mutate(
      .x,
      across(any_of(c(
        "release_version",
        "naaccr_laterality_cd",
        "naaccr_tnm_path_desc",
        "pdl1_iclrange",
        "pdl1_iclrange_2",
        "pdl1_icurange",
        "pdl1_icurange_2",
        "pdl1_tcurange",
        "pdl1_lcpsrange",
        "pdl1_ucpsrange",
        "cpt_seq_date",
        "Match_Norm_Seq_Allele1",
        "Match_Norm_Seq_Allele2",
        "Protein_position"
      )), ~ as.character(.))
    ))

  # name the items in the list
  names(data_releases_pull_data) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
  )

  # for each data release, run create analytic cohort
  # get first object from each item in the list
  # then run create analytic cohort
  data_releases_create_cohort <- map(
    data_releases_pull_data,
    create_analytic_cohort
  )

  # create analytic cohort with return summary = TRUE
  data_releases_create_cohort_with_summary <- map(data_releases_pull_data,
                                                  create_analytic_cohort,
                                                  return_summary = TRUE
  )

  # for some tests, need NSCLC and BrCa (histology tests in create_analytic_cohort)
  # pull a lung and breast cancer data release
  brca_nsclc_pull_data_synapse <- pull_data_synapse(
    cohort = c("BrCa", "NSCLC"),
    version = c("v1.2-consortium", "v3.1-consortium"))

  # for some tests, need two of the same cohort
  # pull data for 2 lung data releases
  # 3.1-consortium is more recent than 2.0-public
  multiple_NSCLC_pull_data_synapse <- pull_data_synapse(
    cohort = c("NSCLC", "NSCLC"),
    version = c("v3.1-consortium", "v2.0-public")
  )

  # pull data for 2 lung and 1 CRC data release
  multiple_NSCLC_CRC_pull_data_synapse <- pull_data_synapse(
    cohort = c("NSCLC", "NSCLC", "CRC"),
    version = c("v3.1-consortium", "v2.0-public", "v2.0-public")
  )
}
