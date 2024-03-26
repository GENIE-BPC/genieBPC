# Tests - No GENIE Access Required ---------------------------------------------
test_that("No specifications- runs with no error", {
  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data
  ), NA)
})

test_that("pull data synapse object is missing", {
  expect_error(create_analytic_cohort())
})

test_that("Institution- argument check", {
  expect_error(msk <- create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    institution = "MSK"
  ), NA)

  expect_equal("MSK", unique(msk[[1]]$institution))

  expect_error(dfci <- create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    institution = "DFCI"
  ), NA)

  expect_equal("DFCI", unique(dfci[[1]]$institution))

  expect_error(vicc <- create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    institution = "VICC"
  ), NA)

  expect_equal("VICC", unique(vicc[[1]]$institution))

  expect_error(uhn <- create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    institution = "UHN"
  ), NA)

  expect_equal("UHN", unique(uhn[[1]]$institution))

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    institution = "non-existant"
  ), "Select from*")
})


test_that("stage_dx- argument check", {

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "Stage I"
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "Stage II"
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "Stage III"
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "Stage IV"
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "staGe IV"
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = c("Stage I", "Stage IV")
  ), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "none"
  ), "*")
})




# Tests - Requiring GENIE Access -----------------------------------------------

# pull data for each cohort
# return to avoid having to re-run pull_data_synapse for
# each test
# pull data for each cohort
# return to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(length(if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {
  # data frame of each release to use for pmap
  data_releases <- synapse_tables %>%
    distinct(cohort, version) %>%
    # define expected number of dataframes based on whether TM and RT data were released
    mutate(expected_n_dfs = case_when(
      # no TM or RT
      cohort == "NSCLC" ~ 11,
      # TM, no RT
      cohort %in% c("CRC", "BrCa") ~ 12,
      # RT, no TM
      cohort == "BLADDER" ~ 12,
      # TM and RT
      cohort %in% c("PANC", "Prostate") ~ 13
    ),
    expected_n_dfs_with_summary = expected_n_dfs + 4)

  # for each data release, pull data into the R environment
  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))
  data_releases_pull_data <- pmap(
      select(data_releases, cohort, version),
        pull_data_synapse)

  # name the items in the list
  names(data_releases_pull_data) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
  )
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {
  # for each data release, run create analytic cohort
  # get first object from each item in the list
  # then run create analytic cohort
  data_releases_create_cohort <- map(data_releases_pull_data, 1) %>%
    map(., create_analytic_cohort)

  # name the items in the list
  names(data_releases_create_cohort) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
  )

  # create analytic cohort with return summary = TRUE
  data_releases_create_cohort_with_summary <- map(data_releases_pull_data, 1) %>%
    map(., create_analytic_cohort, return_summary = TRUE)

  # apply same names to list with summaries
  names(data_releases_create_cohort_with_summary) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
  )
}) > 0)

# will update once we merge in PR to allow multiple cohorts in create_analytic_cohort
test_that("multiple cohorts- argument check", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data[1:2]
  ))
})

test_that("non-existent data_synapse", {
  # a non-existent data_synapse is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$TEST_NONEXIST
  ))
})

test_that("correct number of objects returned from create cohort", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # check that number of items returned is correct
  # data releases with RT and TM
  actual_length <- map_df(data_releases_create_cohort, length) %>%
    pivot_longer(
      cols = everything(),
      names_to = "data_release",
      values_to = "length"
    )

  # compare to expected length
  expect_equal(data_releases$expected_n_dfs, actual_length$length)


  # check thta number of items is also correct when a summary is returned
  actual_length_with_summary <- map_df(data_releases_create_cohort_with_summary, length) %>%
    pivot_longer(
      cols = everything(),
      names_to = "data_release",
      values_to = "length"
    )

  # compare to expected length when return_summary = TRUE
  # +4 for the additional tables returned when return_summary = TRUE
  expect_equal(data_releases$expected_n_dfs_with_summary, actual_length_with_summary$length)

  # check that class is correct
  map2(
    map(data_releases_create_cohort, class),
    rep("list", nrow(data_releases)),
    expect_equal
  )
})


test_that("correct cohort returned from create cohort", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # for each data frame returned with a cohort, get the cohort variable
  # remove genomic data frames since we don't expect them to have a cohort variable
  data_releases_create_cohort_no_genomic <- map(
    data_releases_create_cohort,
    ~ within(
      .x,
      rm(
        cohort_cna,
        cohort_fusions,
        cohort_mutations_extended
      )
    )
  )

  # for each dataframe returned for a data release, get the cohort variable
  cohort_returned <- map_depth(data_releases_create_cohort_no_genomic, select, "cohort",
    .depth = 2
  ) %>%
    map(., bind_rows, .id = "df") %>%
    map(., distinct) %>%
    bind_rows(., .id = "data_release") %>%
    separate(data_release,
      into = c("cohort_expected", "data_release"),
      sep = "_"
    )

  expect_equal(cohort_returned$cohort_expected, cohort_returned$cohort)
})

test_that("check first index cancer default", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # no diagnosis criteria specified
  # expect that the first index cancer is returned without any other
  # incl criteria

  # for each index cancer dataset, pick the first index cancer
  data_releases_create_cohort_ca_dx_index <- map_depth(data_releases_pull_data,
    .depth = 2,
    pluck,
    "ca_dx_index"
  ) %>%
    map_depth(., .depth = 2, group_by, record_id) %>%
    map_depth(., .depth = 2, slice_min, ca_seq) %>%
    map_depth(., .depth = 2, ungroup) %>%
    map(., 1)

  # expect the default from create cohort to match the first index cancer
  # check that the first index cancer diagnosis is returned by create analytic cohort
  map2(
    data_releases_create_cohort_ca_dx_index,
    map(data_releases_create_cohort, "cohort_ca_dx"),
    expect_equal
  )
})

test_that("index_ca_seq", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # not really cohort specific, all cohorts will have index_ca_seq
  # for now, only test on lung
  # first and second index cancer is specified
  # if patient only has 1 index cancer, it should be returned
  # if patient has 2+ index cancers, the first two should be returned
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    index_ca_seq = c(1, 2),
    return_summary = TRUE
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    arrange(cohort, record_id, ca_seq) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    filter(index_ca_seq %in% c(1, 2)) %>%
    select(-index_ca_seq)

  expect_equal(test_1a$cohort_ca_dx, test_1b)


  # an index cancer # that doesn't exist in the data is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    index_ca_seq = 100
  ))

  ## index cancer #s in cohort_ngs match those in cohort_ca_dx
  test2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`CRC_v2.0-public`$CRC_v2.0,
    index_ca_seq = c(1, 2)
  )

  expect_equal(
    test2a$cohort_ca_dx %>%
      select(record_id, ca_seq) %>%
      arrange(record_id, ca_seq),
    test2a$cohort_ngs %>%
      distinct(record_id, ca_seq) %>%
      arrange(record_id, ca_seq)
  )
})

test_that("institution", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # institution is specified and correct institution is returned
  # institution will be available across data releases,
  # don't need to test on each
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    institution = "dfci"
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(institution == "DFCI")

  expect_equal(test_1a$cohort_ca_dx, test_1b)


  # multiple institutions specified
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    institution = c("dfci", "msk")
  )

  test_2b <- data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(institution %in% c("MSK", "DFCI"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # a non-existent institution is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    institution = "uDFCI"
  ))

  # UHN didn't participate in CRC
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`CRC_v2.0-public`$CRC_v2.0,
    institution = "UHN"
  ))
})

test_that("stage_dx", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # stage dx is specified and correct stage is returned
  # not cohort specific, all cohorts will have stage
  # test only on one cohort for now
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    stage_dx = "stage ii"
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx == "Stage II")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # multiple stage values are specified
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    stage_dx = c("Stage I", "stage ii")
  )

  test_2b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx %in% c("Stage I", "Stage II"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # non-existent stage is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    stage_dx = "3A"
  ))
})

test_that("histology", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # no histology is specified, all are returned
  test0b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup()

  expect_equal(
    data_releases_create_cohort$`NSCLC_v2.0-public`$cohort_ca_dx,
    test0b
  )

  # histology is specified and correct histology is returned
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    histology = "adenocarcinoma"
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous == "Adenocarcinoma")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # repeat for BrCa, which has specific histologies available
  test_1c <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    histology = "invasive ductal carcinoma"
  )

  test_1d <- data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_brca == "Invasive ductal carcinoma")

  expect_equal(test_1c$cohort_ca_dx, test_1d)

  # multiple histologies are specified and returned
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    histology = c("adenocarcinoma", "squamous cell")
  )

  test_2b <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous %in% c("Adenocarcinoma", "Squamous cell"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # a non-existent histology is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    histology = "squamous_adeno"
  ))

  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.2-consortium`$BrCa_v1.2,
    histology = "squamous_adeno"
  ))
})

test_that("no regimen specified", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # all regimens are returned
  # should match all regimens given for a patients first index cancer
  test_1b <- inner_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c("cohort", "record_id", "ca_seq")
  )

  expect_equal(
    data_releases_create_cohort$`NSCLC_v2.0-public`$cohort_ca_drugs,
    test_1b
  )
})

test_that("drug regimen specified, order not specified", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # one drug regimen specified, but order not specified
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    filter(regimen_drugs == c("Carboplatin, Pemetrexed Disodium"))

  expect_equal(test_1a$cohort_ca_drugs, test_1b)

  # one drug regimen specified with drugs out of ABC order and in mixed case
  # regimen order not specified
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Pemetrexed DISODIUM, carboplatin")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  # same as above

  expect_equal(test_2a$cohort_ca_drugs, test_1b)

  # multiple drug regimens specified, but order not specified
  test_3a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    filter(regimen_drugs %in% c(
      "Carboplatin, Pemetrexed Disodium",
      "Nivolumab"
    ))

  expect_equal(test_3a$cohort_ca_drugs, test_3b)

  # multiple drug regimens specified, regimen_type = containing
  test_4a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin", "Nivolumab"),
    regimen_type = "containING"
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_4b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    filter(grepl("Carboplatin", regimen_drugs) |
      grepl("Nivolumab", regimen_drugs))

  expect_equal(test_4a$cohort_ca_drugs, test_4b)
})

test_that("drug regimen specified, order specified to be within cancer", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # regimen of a certain number but drug name not specified
  # all patients whose first drug after diagnosis was carbo pem
  test_0a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_0b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c("cohort", "record_id", "ca_seq"),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    slice(which.min(regimen_number)) %>%
    ungroup()

  expect_equal(test_0a$cohort_ca_drugs, test_0b)

  # all patients whose first drug after diagnosis was carbo pem
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c("cohort", "record_id", "ca_seq"),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    slice(which.min(regimen_number)) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium")

  expect_equal(test_1a$cohort_ca_drugs, test_1b)

  # second regimen after diagnosis was carbo pem
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 2,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_2b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
    filter(new_reg_number == 2) %>%
    select(-new_reg_number)

  expect_equal(test_2a$cohort_ca_drugs, test_2b)

  # first AND/OR second regimen after diagnosis was carbo pem
  test_3a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
    filter(new_reg_number %in% c(1, 2)) %>%
    select(-new_reg_number)

  expect_equal(test_3a$cohort_ca_drugs, test_3b)

  # first AND/OR second regimen after diagnosis was carbo pem
  # regimen_type = containing rather than default of exact
  test_4a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  test_4b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(grepl("Carboplatin, Pemetrexed Disodium", regimen_drugs)) %>%
    filter(new_reg_number %in% c(1, 2)) %>%
    select(-new_reg_number)

  expect_equal(test_4a$cohort_ca_drugs, test_4b)
})


test_that("exact drug regimen specified,
          order specified to be within regimen", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # single regimen specified, want first time that regimen
  # was given for all cancers
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id, regimen_drugs) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
    filter(new_reg_number %in% c(1)) %>%
    select(-new_reg_number)

  expect_equal(test_1a$cohort_ca_drugs, test_1b)

  # multiple regimens specified, want first time each given
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_2b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id, regimen_drugs) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(regimen_drugs %in% c(
      "Carboplatin, Pemetrexed Disodium",
      "Nivolumab"
    )) %>%
    filter(new_reg_number %in% c(1)) %>%
    select(-new_reg_number)

  expect_equal(test_2a$cohort_ca_drugs, test_2b)

  # multiple regimens specified
  # first and/or second time they were received
  # multiple regimens specified, want first time each given
  test_3a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1, 2),
    regimen_order_type = "within REGimen"
  )

  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    by = c(
      "cohort", "record_id", "ca_seq"
    ),
    multiple = "all"
  ) %>%
    group_by(record_id, regimen_drugs) %>%
    mutate(new_reg_number = 1:n()) %>%
    ungroup() %>%
    filter(regimen_drugs %in% c(
      "Carboplatin, Pemetrexed Disodium",
      "Nivolumab"
    )) %>%
    filter(new_reg_number %in% c(1, 2)) %>%
    select(-new_reg_number)

  expect_equal(test_3a$cohort_ca_drugs, test_3b)
})

test_that("containing drug regimen specified,
          order specified to be within regimen", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # specify regimen type to be containing (default is exact,
  # which is what is implemented in the above)
  test_1c <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  # order containing
  ordered_containing_regs <- data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs %>%
    filter(grepl("Carboplatin, Pemetrexed Disodium", regimen_drugs)) %>%
    distinct(cohort, record_id, regimen_number, regimen_drugs) %>%
    group_by(cohort, record_id) %>%
    mutate(order_within_containing_regimen = 1:n()) %>%
    ungroup() %>%
    filter(order_within_containing_regimen %in% c(1)) %>%
    select(
      cohort, record_id, regimen_number,
      order_within_containing_regimen
    )

  # merge containing order onto the regimen data
  # only keep regimens of interest
  ca_drugs_with_containing_order <- inner_join(data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_drugs,
    ordered_containing_regs,
    by = c(
      "cohort", "record_id",
      "regimen_number"
    ),
    multiple = "all"
  )

  # merge cohort with patients who received drug regimens of interest
  # in order specified
  test_1d <- inner_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    ca_drugs_with_containing_order,
    by = c(
      "cohort", "record_id", "ca_seq"
    )
  ) %>%
    arrange(cohort, record_id, ca_seq) %>%
    select(
      cohort, record_id, institution,
      regimen_number, ca_seq, everything()
    ) %>%
    as.data.frame()

  expect_equal(
    test_1c$cohort_ca_drugs %>%
      arrange(cohort, record_id, ca_seq) %>%
      select(cohort, record_id, ca_seq, regimen_number,
             everything()),
    test_1d %>%
      arrange(cohort, record_id, ca_seq) %>%
      select(cohort, record_id, ca_seq, regimen_number,
             everything())
  )
})

test_that("regimen_type", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # only testing on a single cancer cohort since not cohort-specific
  # invalid value provided for regimen_type
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_type = "exact_containing"
  ))

  # if regimen_type is specified, regimen_drugs must also be specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`CRC_v2.0-public`$CRC_v2.0,
    regimen_type = "exact"
  ))
})

test_that("regimen_order", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # character value provided for regimen_order
  # only testing on a single cancer cohort since not cohort-specific
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    regimen_order = "C"
  ))
})

test_that("regimen_order_type", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # only testing on a single cancer cohort since not cohort-specific
  # invalid value provided for regimen_order_type
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    regimen_order = 1,
    regimen_order_type =
      "within_btwn_cancer"
  ))

  # regimen_order is specified but regimen_order_type is not
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    regimen_order = 1
  ))

  # regimen_order_type is specified but regimen_order is not
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`$BrCa_v1.1,
    regimen_order_type =
      "within cancer"
  ))
})

test_that("No patients met criteria", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$NSCLC_v2.0,
    regimen_drugs = "Carboplatin, Pemetrexed",
    regimen_order = 1000,
    regimen_order_type = "within cancer"
  ))
})

