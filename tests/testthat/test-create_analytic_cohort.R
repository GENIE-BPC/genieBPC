
# Tests - No GENIE Access Required ---------------------------------------------

test_that("No specifications- runs with no error", {
  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data
  ), NA)
})

test_that("pull data synapse object is missing", {
  expect_error(create_analytic_cohort())
})

# test_that("Institution- argument check", {
#   expect_error(msk <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "MSK"
#   ), NA)
#
#   expect_equal("MSK", unique(msk[[1]]$institution))
#
#   expect_error(dfci <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "DFCI"
#   ), NA)
#
#   expect_equal("DFCI", unique(dfci[[1]]$institution))
#
#   expect_error(vicc <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "VICC"
#   ), NA)
#
#   expect_equal("VICC", unique(vicc[[1]]$institution))
#
#   expect_error(uhn <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "UHN"
#   ), NA)
#
#   expect_equal("UHN", unique(uhn[[1]]$institution))
#
#   expect_error(create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "non-existant"
#   ), "The specified institution*")
# })


# * Check Arguments -----------

# ** Institution ------
test_that("institution - argument check", {
  valid_institutions <- c("MSK", "DFCI", "VICC", "UHN")

  for (inst in valid_institutions) {
    expect_error(
      cohort <- create_analytic_cohort(
        data_synapse = genieBPC::nsclc_test_data,
        institution = inst
      ),
      NA
    )
    expect_equal(inst, unique(cohort[[1]]$institution))
  }

  expect_error(
    create_analytic_cohort(
      data_synapse = genieBPC::nsclc_test_data,
      institution = "non-existant"
    ),
    "The specified institution*"
  )
})

# ** Stage ------
test_that("stage_dx - argument check", {

  valid_stages <- c("Stage I", "Stage II", "Stage III", "Stage IV", "staGe IV")

  for (stage in valid_stages) {
    expect_error(
      create_analytic_cohort(
        data_synapse = genieBPC::nsclc_test_data,
        stage_dx = stage), NA)
    }
})

test_that("stage_dx - two args passed", {

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = c("Stage I", "Stage IV")
  ), NA)
})

test_that("stage_dx - no args passed", {
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
testthat::expect_true(
  if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {
    # data frame of each release to use for pmap
    data_releases <- synapse_tables %>%
      distinct(cohort, version) %>%
      # define expected number of dataframes based on whether TM and RT data were released
      mutate(
        expected_n_dfs = case_when(
          ## no TM or RT
          cohort == "NSCLC" & version %in% c("v1.1-consortium", "v2.0-public") ~ 11,
          # sv file added to NSCLC at 2.2-consortium
          cohort == "NSCLC" ~ 12,
          ## TM, no RT
          cohort %in% c("CRC") & version == "v2.0-public" ~ 12,
          cohort %in% c("BrCa") ~ 12,
          # sv added
          cohort %in% c("CRC") ~ 13,
          ## RT, no TM
          cohort == "BLADDER" & version == "v1.1-consortium" ~ 12,
          # sv added
          cohort == "BLADDER" & version == "v1.2-consortium" ~ 13,
          # TM and RT
          cohort %in% c("PANC", "Prostate") ~ 13
        ),
        expected_n_dfs_with_summary = expected_n_dfs + 4
      )

    # for each data release, pull data into the R environment
    set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))
    data_releases_pull_data <- pmap(
      select(data_releases, cohort, version),
      pull_data_synapse
    ) %>%
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

    length(data_releases_pull_data) > 0
  } else {
    0 == 0
  }
)

testthat::expect_true(
  if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {
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

    length(data_releases_create_cohort_with_summary) > 0
  } else {
    0 == 0
  }
)


test_that("data_synapse - no argument passed", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # a non-existent data_synapse is specified
  expect_error(
    create_analytic_cohort(
      data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`$TEST_NONEXIST))

  expect_error(
    create_analytic_cohort(
      data_synapse = data_releases_pull_data$TEST_NONEXIST))
})


# * General Checks --------------------------------------------------


test_that("correct number of objects returned", {

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
    ~ discard(.x, names(.x) %in% c("cohort_cna",
                                   "cohort_fusions",
                                   "cohort_cV",
                                   "cohort_mutations_extended"))
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
    ) %>%
    # to account for NSCLC2, CRC2, need to remove the number from the cohort var
    mutate(cohort = str_remove_all(pattern = "[:digit:]",
                                   string = cohort))

  expect_equal(cohort_returned$cohort_expected, cohort_returned$cohort)
})

# * Check Arguments --------------------------------------------------

test_that("check first index cancer default", {

  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # no diagnosis criteria specified
  # expect that the first index cancer is returned without any other
  # incl criteria

  # for each index cancer dataset, pick the first index cancer
  data_releases_create_cohort_ca_dx_index <- map(data_releases_pull_data,
    pluck,
    "ca_dx_index"
  ) %>%
    map(., group_by, cohort, record_id) %>%
    map(., slice_min, ca_seq) %>%
    map(., ungroup)

  # expect the default from create cohort to match the first index cancer
  # check that the first index cancer diagnosis is returned by create analytic cohort
  map2(
    data_releases_create_cohort_ca_dx_index,
    map(data_releases_create_cohort, "cohort_ca_dx"),
    expect_equal
  )
})

# ** Index Cancer ------------

# not really cohort specific, all cohorts will have index_ca_seq
# for now, only test on lung
# first and second index cancer is specified
# if patient only has 1 index cancer, it should be returned
# if patient has 2+ index cancers, the first two should be returned
test_that("index_ca_seq - returns correct", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    index_ca_seq = c(1, 2),
    return_summary = TRUE
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    arrange(cohort, record_id, ca_seq) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    filter(index_ca_seq %in% c(1, 2)) %>%
    select(-index_ca_seq)

  expect_equal(test_1a$cohort_ca_dx, test_1b)
})


test_that("index_ca_seq - error when doesn't exist", {

  # an index cancer # that doesn't exist in the data is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    index_ca_seq = 100
  ))
})

test_that("index_ca_seq - results consistent between cohort_ngs and cohort_ngs ", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  test2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`CRC_v2.0-public`,
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


# ** Institution -------------------

# institution is specified and correct institution is returned
# institution will be available across data releases,
# don't need to test on each

test_that("institution - argument check 1 or more ", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # --- Single institution (DFCI, NSCLC) ---
  dfci_cohort <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    institution = "dfci"
  )

  dfci_expected <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    filter(institution == "DFCI")

  expect_equal(dfci_cohort$cohort_ca_dx, dfci_expected)

  # --- Multiple institutions (MSK + DFCI, BrCa) ---
  msk_dfci_cohort <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`,
    institution = c("dfci", "msk")
  )

  msk_dfci_expected <- data_releases_pull_data$`BrCa_v1.1-consortium`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    filter(institution %in% c("MSK", "DFCI"))

  expect_equal(msk_dfci_cohort$cohort_ca_dx, msk_dfci_expected)

})

test_that("institution - non-existent or incorrect specified", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # a non-existent institution is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    institution = "uDFCI"
  ))

  # UHN didn't participate in CRC
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`CRC_v2.0-public`,
    institution = "UHN"
  ),
  "The specified institution is not available*")

  # institution specified, only available for 1 of several cohorts
  expect_message(create_analytic_cohort(
               data_synapse = data_releases_pull_data %>%
                 purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
               institution = "UHN"),
               "The specified institution did not contribute data*")
})

# ** Stage -------------------

# stage dx is specified and correct stage is returned
# not cohort specific, all cohorts will have stage
# test only on one cohort for now
test_that("stage_dx", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    stage_dx = "stage ii"
  )

  test_1b <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx == "Stage II")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # multiple stage values are specified
  test_2a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    stage_dx = c("Stage I", "stage ii")
  )

  test_2b <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx %in% c("Stage I", "Stage II"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # non-existent stage is specified
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    stage_dx = "3A"
  ))
})

# ** Histology -------------------

# ---- No histology specified
test_that("histology - Returns all records when no histology is specified", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expected <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup()

  result <- data_releases_create_cohort$`NSCLC_v2.0-public`$cohort_ca_dx

  expect_equal(result, expected)
})

# ---- Histology specified (single)

test_that("histology - correct returned for adenocarcinoma", {
  result <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    histology = "adenocarcinoma"
  )

  expected <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous == "Adenocarcinoma")

  expect_equal(result$cohort_ca_dx, expected)
})

test_that("histology - correct returned for BrCa invasive ductal carcinoma", {
  result <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.1-consortium`,
    histology = "invasive ductal carcinoma"
  )

  expected <- data_releases_pull_data$`BrCa_v1.1-consortium`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    filter(ca_hist_brca == "Invasive ductal carcinoma")

  expect_equal(result$cohort_ca_dx, expected)
})

# ---- Multiple histologies

test_that("histology- Multiple histologies are returned correctly", {


  result <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    histology = c("adenocarcinoma", "squamous cell")
  )

  expected <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous %in% c("Adenocarcinoma", "Squamous cell"))

  expect_equal(result$cohort_ca_dx, expected)
})


# ---- Invalid histology
test_that("histology - errors for non-existent histology (NSCLC and BrCa)", {
  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    histology = "squamous_adeno"
  ))

  expect_error(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`BrCa_v1.2-consortium`,
    histology = "squamous_adeno"
  ))
})




# ---- Cross-cohort histology filtering

test_that("BrCa + non-BrCa data, BrCa histology specified", {

  result <- create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    histology = "Invasive ductal carcinoma"
  )

  expected <- data_releases_pull_data$`BrCa_v1.2-consortium`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq) %>%
    ungroup() %>%
    filter(ca_hist_brca == "Invasive ductal carcinoma") %>%
    pull(ca_hist_brca)

  expect_equal(result$cohort_ca_dx$ca_hist_brca, expected)
})


test_that("BrCa + non-BrCa data, NSCLC histology specified", {
  result <- create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep( names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    histology = "Adenocarcinoma"
  )

  expected <- data_releases_pull_data$`NSCLC_v1.1-consortium`$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous == "Adenocarcinoma") %>%
    pull(ca_hist_adeno_squamous)

  expect_equal(result$cohort_ca_dx$ca_hist_adeno_squamous, expected)
})


test_that("BrCa + NSCLC, both histologies specified", {
  result <- create_analytic_cohort(
    data_synapse =data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    histology = c("Invasive ductal carcinoma", "Adenocarcinoma")
  )$cohort_ca_dx %>%
    arrange(cohort, record_id) %>%
    select(ca_hist_brca, ca_hist_adeno_squamous)

  expected <- bind_rows(
    data_releases_pull_data$`NSCLC_v1.1-consortium`$ca_dx_index,
    data_releases_pull_data$`BrCa_v1.2-consortium`$ca_dx_index
  ) %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq) %>%
    ungroup() %>%
    filter(
      (cohort == "BrCa" & ca_hist_brca == "Invasive ductal carcinoma") |
        (cohort == "NSCLC" & ca_hist_adeno_squamous == "Adenocarcinoma")
    ) %>%
    arrange(cohort, record_id) %>%
    select(ca_hist_brca, ca_hist_adeno_squamous)

  expect_equal(result, expected)
})


# ** Regimens -------------------------------------------------------------

test_that("no regimen specified", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # all regimens are returned
  # should match all regimens given for a patients first index cancer
  test_1b <- inner_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Pemetrexed DISODIUM, carboplatin")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  # same as above

  expect_equal(test_2a$cohort_ca_drugs, test_1b)

  # multiple drug regimens specified, but order not specified
  test_3a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin", "Nivolumab"),
    regimen_type = "containING"
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_4b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_0b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
    by = c("cohort", "record_id", "ca_seq"),
    multiple = "all"
  ) %>%
    group_by(record_id) %>%
    slice(which.min(regimen_number)) %>%
    ungroup()

  expect_equal(test_0a$cohort_ca_drugs, test_0b)

  # all patients whose first drug after diagnosis was carbo pem
  test_1a <- create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 2,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_2b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  test_4b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      ungroup() %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_1b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_2b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1, 2),
    regimen_order_type = "within REGimen"
  )

  test_3b <- left_join(
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
      group_by(record_id) %>%
      slice(which.min(ca_seq)) %>%
      select(cohort, record_id, ca_seq),
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  # order containing
  ordered_containing_regs <- data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs %>%
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
  ca_drugs_with_containing_order <- inner_join(data_releases_pull_data$`NSCLC_v2.0-public`$ca_drugs,
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
    data_releases_pull_data$`NSCLC_v2.0-public`$ca_dx_index %>%
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
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
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

  # when a single cohort is supplied
  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data$`NSCLC_v2.0-public`,
    regimen_drugs = "Carboplatin, Pemetrexed",
    regimen_order = 1000,
    regimen_order_type = "within cancer"
  ))

  # message returned for criteria not met by all cohorts: histology
  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    histology = "Adenocarcinoma"
  ), "Note: No patients meeting the specified criteria*")

  # message returned for criteria not met by all cohorts: institution
  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    institution = "UHN"
  ))

  # message returned for criteria not met by any cohorts
  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v1.1-consortium", "BrCa_v1.2-consortium")),
    regimen_drugs = "ABC",
  ))
})


# Multiple Cohort Tests ---------------------------------------------------

test_that("multiple cohorts - message is thrown", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # message returned when multiple data releases are supplied for the same cohort
  expect_message(create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v3.1-consortium", "NSCLC_v2.0-public")),
  ), "Note: Multiple data releases were supplied*")
})

# check that only most recent release is returned
test_that("multiple cohorts - check most recent release is returned", {

  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_equal(create_analytic_cohort(
    data_synapse = data_releases_pull_data %>%
      purrr::keep(names(.) %in% c("NSCLC_v3.1-consortium", "NSCLC_v2.0-public"))) %>%
      pluck("cohort_ca_dx") %>%
      select(-cohort_release),

    data_releases_pull_data$`NSCLC_v3.1-consortium`$ca_dx_index %>%
      group_by(cohort, record_id) %>%
      slice_min(ca_seq) %>%
      ungroup())
})

# check that only most recent release is returned, with multiple of one cohort
# AND another cohort specified
test_that("multiple cohorts - most recent release is returned when multiple cohorts are provided", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  selected_cohorts <- data_releases_pull_data %>%
    purrr::keep(names(.) %in% c("NSCLC_v3.1-consortium",
                                "NSCLC_v2.0-public",
                                "CRC_v2.0-public"))

  result <- create_analytic_cohort(data_synapse = selected_cohorts) %>%
    pluck("cohort_ca_dx") %>%
    select(-cohort_release) %>%
    select(order(colnames(.))) %>%
    arrange(cohort, record_id)

  expected <- bind_rows(
    data_releases_pull_data$`NSCLC_v3.1-consortium`$ca_dx_index,
    data_releases_pull_data$`CRC_v2.0-public`$ca_dx_index
  ) %>%
    group_by(cohort, record_id) %>%
    slice_min(ca_seq, with_ties = FALSE) %>%
    ungroup() %>%
    select(order(colnames(.))) %>%
    arrange(cohort, record_id)

  expect_equal(result, expected)
})


# check that the total number of rows per dataframe are equal when pulling
# individual cohorts and stacking as pulling multiple cohorts
test_that("multiple cohorts- names & number of rows per dataframe", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  most_recent_release_versions <- synapse_version(most_recent = TRUE) %>%
    mutate(cohort_version = paste0(cohort, "_", version)) %>%
    select(cohort_version) %>%
    unlist()

  ## get unique pairs of cohorts to compare
  pairs_most_recent_release_versions <- combn(most_recent_release_versions, 2)

  ## create empty lists to store results from foor loop
  dim_individual_cohorts_list <- list()
  dim_multiple_cohorts_list <- list()

  df_names_individual_cohorts_list <- list()
  df_names_multiple_cohorts_list <- list()

  ## loop through all unique pairs of cohorts
  for (i in 1:(ncol(pairs_most_recent_release_versions))) {
    dim_individual_cohorts <- full_join(
      create_analytic_cohort(
        data_synapse = data_releases_pull_data %>%
          purrr::keep(
            names(.) %in% c(pairs_most_recent_release_versions[1, i])
            )) %>%
        map_df(., ~ tibble(
          df_name = deparse(substitute(.x)), n_row = nrow(.x)
        ), .id = "df_name"),
      create_analytic_cohort(
        data_synapse = data_releases_pull_data %>%
          purrr::keep(
            names(.) %in% c(pairs_most_recent_release_versions[2, i])
            )) %>%
        map_df(., ~ tibble(
          df_name = deparse(substitute(.x)), n_row = nrow(.x)
        ), .id = "df_name"),
      by = "df_name"
    ) %>%
      mutate(
        total_rows = select(., contains("n_row")) %>% rowSums(na.rm = TRUE),
        cohort_names = paste0(sort(
          c(
            pairs_most_recent_release_versions[1, i],
            pairs_most_recent_release_versions[2, i]
          )
        ), collapse = ", ")
      )

    df_names_individual_cohorts <- c(
      data_releases_pull_data %>%
        purrr::keep(names(.) %in% c(pairs_most_recent_release_versions[1, i])) %>%
        flatten() %>%
        names(),
      data_releases_pull_data %>%
        purrr::keep(names(.) %in% c(pairs_most_recent_release_versions[2, i])) %>%
        flatten() %>%
        names()
    ) %>%
      unique()

    dim_multiple_cohorts <- create_analytic_cohort(
      data_synapse = data_releases_pull_data %>%
        purrr::keep(
          names(.) %in% c(
            pairs_most_recent_release_versions[1, i],
            pairs_most_recent_release_versions[2, i]
            ))) %>%
      map_df(., ~ tibble(
        df_name = deparse(substitute(.x)),
        total_rows = nrow(.x)
      ), .id = "df_name") %>%
      mutate(cohort_names = paste0(sort(
        c(
          pairs_most_recent_release_versions[1, i],
          pairs_most_recent_release_versions[2, i]
        )
      ), collapse = ", "))

    df_names_multiple_cohorts <- c(
      data_releases_pull_data %>%
        purrr::keep(
          names(.) %in% c(
            pairs_most_recent_release_versions[1, i],
            pairs_most_recent_release_versions[2, i]
          )
        ) %>%
        flatten() %>%
        names()
    ) %>% unique()

    ## store results in list objects
    dim_individual_cohorts_list[[i]] <- data.frame(
      dim_individual_cohorts %>%
        select(cohort_names, df_name, total_rows) %>%
        arrange(df_name)
    )

    df_names_individual_cohorts_list[[i]] <- sort(df_names_individual_cohorts)

    dim_multiple_cohorts_list[[i]] <- data.frame(
      dim_multiple_cohorts %>%
        select(cohort_names, df_name, total_rows) %>%
        arrange(df_name)
    )

    df_names_multiple_cohorts_list[[i]] <- sort(df_names_multiple_cohorts)

  }

  expect_equal(dim_individual_cohorts_list, dim_individual_cohorts_list)
  expect_equal(df_names_individual_cohorts_list, df_names_multiple_cohorts_list)

})
