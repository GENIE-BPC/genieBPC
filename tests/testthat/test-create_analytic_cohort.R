# Tests - No GENIE Access Required ---------------------------------------------
test_that("No specifications- runs with no error", {

  # one cohort (must be nested list)
  expect_error(create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data)), NA)

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data), "The data_synapse*")

  expect_error(create_analytic_cohort(
    data_synapse = list(list(genieBPC::nsclc_test_data),
                        genieBPC::nsclc_test_data), "The data_synapse*"
  ))

})

test_that("Institution- argument check", {

  # expect_error(create_analytic_cohort(
  #   data_synapse = list(genieBPC::nsclc_test_data),
  #   institution = "DFCI"), NA)
  testthat::skip_if_not(.is_connected_to_genie())

  data_nsclc <- list(genieBPC::nsclc_test_data)

  expect_error(create_analytic_cohort(
    data_synapse = data_nsclc,
    institution = "VICC"), NA)

  expect_error(create_analytic_cohort(
    data_synapse = data_nsclc,
    institution = "DFCI"), NA)

  expect_error(create_analytic_cohort(
    data_synapse = data_nsclc,
    institution = "UHN"), NA)

  expect_error(create_analytic_cohort(
    data_synapse = data_nsclc,
    institution = c("UHN", "wrong")), "Select from *")

  expect_error(create_analytic_cohort(
    data_synapse = pull_data_synapse("CRC", "v2.0-public"),
    institution = "UHN"),
    "Select from available participating instit*")

  # test one good one bad inst for BrCa
  expect_error(create_analytic_cohort(
    data_synapse = pull_data_synapse("BrCa", "v1.2-consortium"),
    institution = c("UHN", "VICC")),
    "Select from available participating institutions. For CRC/BrCa*")

  expect_error(create_analytic_cohort(
    data_synapse = pull_data_synapse("BrCa", "v1.2-consortium"),
    institution = c("MSK", "VICC")), NA)

  expect_error(create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data),
    institution = "non-existant"),
    "Select from available participating institutions. For NSCLC*")

  # check if accepted for more than one cohort
  # we want UHN for NSCLC but not for CRC with no error

  data_crc <- pull_data_synapse("CRC", "v2.0-public")

  expect_error(test_two <- create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data, data_crc[[1]]),
    institution = c("UHN", "MSK")), NA)

  look_at <- test_two %>%
    pluck(1) %>%
    select(cohort, institution)%>%
    count(cohort, institution)

  expect_equal(look_at %>% filter(cohort == "CRC") %>% nrow(), 1)
  expect_equal(look_at %>% filter(cohort == "NSCLC") %>% nrow(), 2)
  expect_true(look_at %>% filter(cohort == "CRC") %>% pull(institution)%>%
                unique() == "MSK")

})


test_that("stage_dx- argument check", {

  expect_error(create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data),
    stage_dx = "Stage IV"), NA)

  expect_equal("Stage IV", create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data),
    stage_dx = "Stage IV") %>% pluck(2) %>%
      pull(stage_dx) %>% unique())

  expect_error(create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data),
    stage_dx = "none"), "Select from available stages*")

})




# Tests - Requiring GENIE Access -----------------------------------------------
# # test that a list of three or seven datasets are returned
# # from create_analytic_cohort

# pull data for each cohort
# return to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(length(if (.is_connected_to_genie()) {
  nsclc_data <- pull_data_synapse("NSCLC",
                                  version = "v1.1-consortium"
  )
} else {
  nsclc_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  crc_data <- pull_data_synapse("CRC",
                                version = "v1.1-consortium"
  )
} else {
  crc_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  brca_data <- pull_data_synapse("BrCa",
                                 version = "v1.1-consortium"
  )
} else {
  brca_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  bladder_data <- pull_data_synapse("BLADDER",
                                 version = "v1.1-consortium"
  )
} else {
  bladder_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  panc_data <- pull_data_synapse("PANC",
                                    version = "v1.2-consortium"
  )
} else {
  panc_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  prost_data <- pull_data_synapse("Prostate",
                                 version = "v1.2-consortium"
  )
} else {
  prost_data <- list("a")
}) > 0)

test_that("multiple cohorts- argument check", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # pull two cohorts together directly using pull_synapse
  two_cohorts <- pull_data_synapse(cohort = c("NSCLC", "CRC"),
                                   version = c("v1.1-consortium",
                                               "v1.1-consortium"))

  expect_error(test_two <- create_analytic_cohort(
    data_synapse = two_cohorts), NA
  )

  expect_true(unique(unique(test_two[[1]]$cohort) == c("CRC", "NSCLC")))

  # 2+ cohorts pulled separately and provided as a nested list obj

  expect_error(two_list <- create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data, crc_data)
  ), "The data_synapse *")

  expect_error(two_list <- create_analytic_cohort(
    data_synapse = list(genieBPC::nsclc_test_data, crc_data$CRC_v1.1)
  ), NA)

  expect_true(all(unique(two_list[[1]]$cohort) %in% c("CRC", "NSCLC")))


})


test_that("correct number of objects returned from create cohort", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # NSCLC
  test1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  expect_equal(length(test1a), 11)
  expect_equal(class(test1a), "list")

  test1b <- create_analytic_cohort(
    data_synapse = nsclc_data,
    return_summary = TRUE
  )

  expect_equal(length(test1b), 15)
  expect_equal(class(test1b), "list")

  # CRC
  test2a <- create_analytic_cohort(
    data_synapse = crc_data,
    return_summary = FALSE
  )

  expect_equal(length(test2a), 12)
  expect_equal(class(test2a), "list")

  test2b <- create_analytic_cohort(
    data_synapse = crc_data,
    return_summary = TRUE
  )

  expect_equal(length(test2b), 16)
  expect_equal(class(test2b), "list")

  # repeat for BrCa
  test3 <- create_analytic_cohort(
    data_synapse = brca_data,
    return_summary = FALSE
  )

  expect_equal(length(test3), 12)
  expect_equal(class(test3), "list")

  test3 <- create_analytic_cohort(
    data_synapse = brca_data,
    return_summary = TRUE
  )

  expect_equal(length(test3), 16)
  expect_equal(class(test3), "list")

  # repeat for multi-cohort case with one cohort having no tumor_marker data
  test4a <- create_analytic_cohort(
    data_synapse = list(crc_data$CRC_v1.1, nsclc_data$NSCLC_v1.1),
    return_summary = FALSE
  )

  test4b <- create_analytic_cohort(
    data_synapse = list(crc_data$CRC_v1.1, nsclc_data$NSCLC_v1.1),
    return_summary = TRUE
  )

  test4c <- create_analytic_cohort(
    data_synapse = list(bladder_data$BLADDER_v1.1, nsclc_data$NSCLC_v1.1),
    return_summary = TRUE
  )

  test4d <- create_analytic_cohort(
    data_synapse = list(prost_data$Prostate_v1.2, panc_data$PANC_v1.2),
    return_summary = FALSE
  )

  expect_equal(length(test4a), 12)
  expect_equal(length(test4b), 16)
  expect_equal(length(test4c), 16)
  expect_equal(length(test4d), 13)

  expect_false("cohort_tumor_marker" %in% names(test4c))
  expect_true("cohort_ca_radtx" %in% names(test4c))

  expect_equal(unique(test4a$cohort_tumor_marker$cohort), "CRC")
  expect_equal(unique(test4c$cohort_ca_radtx$cohort), "BLADDER")
  expect_true(all(unique(test4d$cohort_tumor_marker$cohort %in%
                           c("Prostate", "PANC"))))


})

test_that("pull data synapse object is missing", {
  expect_error(create_analytic_cohort())
})

test_that("correct cohort returned from create cohort", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  test1 <- create_analytic_cohort(
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  expect_equal(unique(test1$cohort_ca_dx$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_ca_drugs$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_ngs$cohort), "NSCLC")

  # check CRC
  test2 <- create_analytic_cohort(
    data_synapse = crc_data,
    return_summary = FALSE
  )

  expect_equal(unique(test2$cohort_ca_dx$cohort), "CRC")
  expect_equal(unique(test2$cohort_ca_drugs$cohort), "CRC")
  expect_equal(unique(test2$cohort_ngs$cohort), "CRC")
})

test_that("cohort and data_synapse", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # no diagnosis criteria specified
  # expect that the first index cancer is returned without any other
  # incl criteria
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data
  )

  test_1b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup()

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # a non-existent data_synapse is specified
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data$TEST_NONEXIST
  ))
})

test_that("index_ca_seq", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # first and second index cancer is specified
  # if patient only has 1 index cancer, it should be returned
  # if patient has 2+ index cancers, the first two should be returned
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    index_ca_seq = c(1, 2),
    return_summary = TRUE
  )

  test_1b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    arrange(cohort, record_id, ca_seq) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    filter(index_ca_seq %in% c(1, 2)) %>%
    select(-index_ca_seq)

  expect_equal(test_1a$cohort_ca_dx, test_1b)


  # an index cancer # that doesn't exist in the data is specified
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data,
    index_ca_seq = 100
  ))

  ## index cancer #s in cohort_ngs match those in cohort_ca_dx
  test2a <- create_analytic_cohort(
    data_synapse = crc_data,
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
  testthat::skip_if_not(.is_connected_to_genie())

  # institution is specified and correct institution is returned
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    institution = "dfci"
  )

  test_1b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(institution == "DFCI")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # multiple institutions specified
  test_2a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    institution = c("dfci", "msk")
  )

  test_2b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(institution %in% c("MSK", "DFCI"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # a non-existent institution is specified
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data,
    institution = "uDFCI"
  ))


  expect_error(create_analytic_cohort(
    data_synapse = crc_data,
    institution = "UHN"
  ))
})

test_that("stage_dx", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # stage dx is specified and correct stage is returned
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    stage_dx = "stage ii"
  )

  test_1b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx == "Stage II")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # multiple stage values are specified
  test_2a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    stage_dx = c("Stage I", "stage ii")
  )

  test_2b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(stage_dx %in% c("Stage I", "Stage II"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # non-existent stage is specified
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data,
    stage_dx = "3A"
  ))
})

test_that("histology", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # no histology is specified, call are returned
  test0a <- create_analytic_cohort(
    data_synapse = nsclc_data
  )

  test0b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup()

  expect_equal(test0a$cohort_ca_dx, test0b)

  # repeat for brca
  test0c <- create_analytic_cohort(
    data_synapse = brca_data
  )

  test0d <- brca_data$BrCa_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup()

  expect_equal(test0c$cohort_ca_dx, test0d)

  # histology is specified and correct histology is returned
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    histology = "adenocarcinoma"
  )

  test_1b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous == "Adenocarcinoma")

  expect_equal(test_1a$cohort_ca_dx, test_1b)

  # repeat for BrCa
  test_1c <- create_analytic_cohort(
    data_synapse = brca_data,
    histology = "invasive ductal carcinoma"
  )

  test_1d <- brca_data$BrCa_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_brca == "Invasive ductal carcinoma")

  expect_equal(test_1c$cohort_ca_dx, test_1d)

  # multiple histologies are specified and returned
  test_2a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    histology = c("adenocarcinoma", "squamous cell")
  )

  test_2b <- nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous %in% c("Adenocarcinoma", "Squamous cell"))

  expect_equal(test_2a$cohort_ca_dx, test_2b)

  # a non-existent histology is specified
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data,
    histology = "squamous_adeno"
  ))

  expect_error(create_analytic_cohort(
    data_synapse = brca_data,
    histology = "squamous_adeno"
  ))
})

test_that("no regimen specified", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # all regimens are returned
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  # should match all regimens given for a patients first index cancer
  test_1b <- inner_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
  by = c("cohort", "record_id", "ca_seq")
  )

  expect_equal(test_1a$cohort_ca_drugs, test_1b)
})

test_that("drug regimen specified, order not specified", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # one drug regimen specified, but order not specified
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_1b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
  by = c(
    "cohort", "record_id", "ca_seq"
  ),
  multiple = "all"
  ) %>%
    filter(regimen_drugs == c("Carboplatin, Pemetrexed Disodium"))

  expect_equal(test_1a$cohort_ca_drugs, test_1b)

  # also expect only diagnoses to patients who received this drug regimen
  # to be returned
  ### have to come back here

  # one drug regimen specified with drugs out of ABC order and in mixed case
  # regimen order not specified
  test_2a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Pemetrexed DISODIUM, carboplatin")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  # same as above

  expect_equal(test_2a$cohort_ca_drugs, test_1b)

  # multiple drug regimens specified, but order not specified
  test_3a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab")
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_3b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin", "Nivolumab"),
    regimen_type = "containING"
  )

  # expect all times that drug was received (for the first index ca)
  # to be returned
  test_4b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
  testthat::skip_if_not(.is_connected_to_genie())

  # regimen of a certain number but drug name not specified
  # all patients whose first drug after diagnosis was carbo pem
  test_0a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_0b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
  by = c("cohort", "record_id", "ca_seq"),
  multiple = "all"
  ) %>%
    group_by(record_id) %>%
    slice(which.min(regimen_number)) %>%
    ungroup()

  expect_equal(test_0a$cohort_ca_drugs, test_0b)

  # all patients whose first drug after diagnosis was carbo pem
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 1,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_1b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = 2,
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_2b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "Exact",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  # compare to data
  test_3b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1, 2),
    regimen_order_type = "within cancer"
  )

  test_4b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
  testthat::skip_if_not(.is_connected_to_genie())

  # single regimen specified, want first time that regimen
  # was given for all cancers
  test_1a <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_1b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  test_2b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
    regimen_order = c(1, 2),
    regimen_order_type = "within REGimen"
  )

  test_3b <- left_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    select(cohort, record_id, ca_seq),
  nsclc_data$NSCLC_v1.1$ca_drugs,
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
  testthat::skip_if_not(.is_connected_to_genie())

  # specify regimen type to be containing (default is exact,
  # which is what is implemented in the above)
  test_1c <- create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_type = "containing",
    regimen_order = c(1),
    regimen_order_type = "within REGimen"
  )

  # order containing
  ordered_containing_regs <- nsclc_data$NSCLC_v1.1$ca_drugs %>%
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
  ca_drugs_with_containing_order <- inner_join(nsclc_data$NSCLC_v1.1$ca_drugs,
    ordered_containing_regs,
    by = c(
      "cohort", "record_id",
      "regimen_number"
    ),
    multiple = "all"
  )

  # merge cohort with patients who received drug regimens of interest
  # in order specified
  test_1d <- inner_join(nsclc_data$NSCLC_v1.1$ca_dx_index %>%
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
      arrange(cohort, record_id, ca_seq),
    test_1d
  )
})

test_that("regimen_type", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # invalid value provided for regimen_type
  expect_error(create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_type = "exact_containing"
  ))

  # if regimen_type is specified, regimen_drugs must also be specified
  expect_error(create_analytic_cohort(
    data_synapse = crc_data,
    regimen_type = "exact"
  ))
})

test_that("regimen_order", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # character value provided for regimen_order
  expect_error(create_analytic_cohort(
    data_synapse = brca_data,
    regimen_order = "C"
  ))
})

test_that("regimen_order_type", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  # invalid value provided for regimen_order_type
  expect_error(create_analytic_cohort(
    data_synapse = brca_data,
    regimen_order = 1,
    regimen_order_type =
      "within_btwn_cancer"
  ))

  # regimen_order is specified but regimen_order_type is not
  expect_error(create_analytic_cohort(
    data_synapse = brca_data,
    regimen_order = 1
  ))

  # regimen_order_type is specified but regimen_order is not
  expect_error(create_analytic_cohort(
    data_synapse = brca_data,
    regimen_order_type =
      "within cancer"
  ))
})

test_that("No patients met criteria", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  expect_message(create_analytic_cohort(
    data_synapse = nsclc_data,
    regimen_drugs = "Carboplatin, Pemetrexed",
    regimen_order = 100,
    regimen_order_type = "within cancer"
  ))
})
