# pull data for each cohort
# return to to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(length(if (.is_connected_to_genie()) {
  nsclc_data <- pull_data_synapse("NSCLC",
    version = "v2.0-public"
  )

  nsclc_cohort <- create_analytic_cohort(data_synapse = nsclc_data$NSCLC_v2.0)
} else {
  nsclc_data <- list("a")
}) > 0)

testthat::expect_true(length(if (.is_connected_to_genie()) {
  crc_data <- pull_data_synapse("CRC",
    version = "v2.0-public"
  )
} else {
  crc_data <- list("a")
}) > 0)

test_that("missing data_cohort", {
  expect_error(select_unique_ngs())
})

test_that("non-existant oncotree code", {
  expect_error(select_unique_ngs(
    data_cohort = nsclc_data$NSCLC_v2.0,
    oncotree_code = "GENIE"
  ))
})

test_that("min/max time", {
  # a valid value is given
  expect_error(select_unique_ngs(
    data_cohort = nsclc_cohort,
    min_max_time = "abc"
  ))

  # only 1 value is given
  expect_error(select_unique_ngs(
    data_cohort = nsclc_cohort,
    min_max_time = c("min", "max")
  ))
})

test_that("sample_type", {
  # a valid value is given
  expect_error(select_unique_ngs(
    data_cohort = nsclc_cohort,
    sample_type = "abc"
  ))

  # only 1 value is given
  expect_error(select_unique_ngs(
    data_cohort = nsclc_cohort,
    sample_type = c("primary", "local")
  ))
})

test_that("function returns unique sample for each record", {
  skip_if_not(genieBPC:::.is_connected_to_genie())


  # NSCLC #
  ### all samples ###
  cohort_temp <- create_analytic_cohort(
    data_synapse = nsclc_data$NSCLC_v2.0,
    return_summary = FALSE
  )

  test1 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs
  )
  expect_true(tibble::is_tibble(test1))
  expect_equal(ncol(test1), 28)
  expect_equal(nrow(test1), 1846)
  expect_equal(
    length(unique(test1$cpt_genie_sample_id)),
    length(unique(test1$record_id))
  )


  ### Stage IV ###
  cohort_temp <- create_analytic_cohort(
    stage_dx = c("Stage IV"),
    data_synapse = nsclc_data$NSCLC_v2.0,
    return_summary = FALSE
  )


  test2 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs
  )
  expect_true(tibble::is_tibble(test2))
  expect_equal(ncol(test2), 28)
  expect_equal(nrow(test2), 797)
  expect_equal(
    length(unique(test2$cpt_genie_sample_id)),
    length(unique(test2$record_id))
  )

  ### DFCI only ###
  cohort_temp <- create_analytic_cohort(
    data_synapse = nsclc_data$NSCLC_v2.0,
    return_summary = FALSE,
    institution = "DFCI"
  )

  test3 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs
  )
  expect_true(tibble::is_tibble(test3))
  expect_equal(ncol(test3), 28)
  expect_equal(nrow(test3), 696)
  expect_equal(unique(test3$institution), "DFCI")


  ### Check patient ###
  ##### Local min #####
  test4 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Local",
    min_max_time = "min"
  )
  expect_true(tibble::is_tibble(test4))
  expect_equal(ncol(test4), 28)
  expect_equal(nrow(test4), 696)
  expect_equal(unique(test4$institution), "DFCI")
  expect_equal(
    as.character(test4[
      test4$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("31.5131578947368", "Local recurrence")
  )

  ##### Local max #####
  test5 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Local",
    min_max_time = "max"
  )
  expect_true(tibble::is_tibble(test5))
  expect_equal(ncol(test5), 28)
  expect_equal(nrow(test5), 696)
  expect_equal(unique(test5$institution), "DFCI")
  expect_equal(
    as.character(test5[
      test5$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("44.0789473684211", "Local recurrence")
  )


  ##### Met min/max (are the same) #####
  test6 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Metastasis",
    min_max_time = "max"
  )
  expect_true(tibble::is_tibble(test6))
  expect_equal(ncol(test6), 28)
  expect_equal(nrow(test6), 696)
  expect_equal(unique(test6$institution), "DFCI")
  expect_equal(
    as.character(test6[
      test6$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("31.5131578947368", "Metastasis site unspecified")
  )
})
