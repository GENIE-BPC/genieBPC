test_that("missing data_cohort", {
  expect_error(select_unique_ngs())
})

test_that("non-existant oncotree code", {
  expect_error(select_unique_ngs(data_cohort = nsclc_cohort$cohort_ca,
                                 oncotree_code = "GENIE"))
})

test_that("min/max time", {
  # a valid value is given
  expect_error(select_unique_ngs(data_cohort = nsclc_cohort$cohort_ca,
                                 min_max_time = "abc"))

  # only 1 value is given
  expect_error(select_unique_ngs(data_cohort = nsclc_cohort$cohort_ca,
                                 min_max_time = c("min", "max")))
})

test_that("sample_type", {
  # a valid value is given
  expect_error(select_unique_ngs(data_cohort = nsclc_cohort$cohort_ca,
                                 sample_type = "abc"))

  # only 1 value is given
  expect_error(select_unique_ngs(data_cohort = nsclc_cohort$cohort_ca,
                                 sample_type = c("primary", "local")))
})

test_that("function returns unique sample for each record", {
  # exit if user doesn't have synapser, a log in, or access to data.
  skip_if_not_installed("synapser", minimum_version = NULL)
  skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
          "Not logged into Synapse")
  skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
          "Not able to access the data")

  # run here to avoid having to run within each test
  nsclc_data <- pull_data_synapse("NSCLC", version = "1.1-consortium")
  crc_data <- pull_data_synapse(c("CRC"), version = "1.1-consortium")

  objs <- list("nsclc_data" = nsclc_data,
               "crc_data" = crc_data)

  list2env(objs, envir = .GlobalEnv)

  # NSCLC #
  ### all samples ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  expect_warning(test1 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs))
  expect_true(tibble::is_tibble(test1))
  expect_equal(ncol(test1), 20)
  expect_equal(nrow(test1), 1849)
  expect_equal(length(unique(test1$cpt_genie_sample_id)),
               length(unique(test1$record_id)))


  ### Stage IV ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    stage_dx = c("Stage IV"),
    data_synapse = nsclc_data,
    return_summary = FALSE
  )


  expect_warning(test2 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs))
  expect_true(tibble::is_tibble(test2))
  expect_equal(ncol(test2), 20)
  expect_equal(nrow(test2), 793)
  expect_equal(length(unique(test2$cpt_genie_sample_id)),
               length(unique(test2$record_id)))

  ### DFCI only ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    data_synapse = nsclc_data,
    return_summary = FALSE,
    institution = "DFCI"
  )

  expect_warning(test3 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs))
  expect_true(tibble::is_tibble(test3))
  expect_equal(ncol(test3), 20)
  expect_equal(nrow(test3), 699)
  expect_equal(unique(test3$institution), "DFCI")


  ### Check patient ###
  ##### Local min #####
  expect_warning(test4 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Local",
    min_max_time = "min"
  ))
  expect_true(tibble::is_tibble(test4))
  expect_equal(ncol(test4), 20)
  expect_equal(nrow(test4), 699)
  expect_equal(unique(test4$institution), "DFCI")
  expect_equal(
    as.character(test4[
      test4$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("31.5131578947368", "Local recurrence")
  )

  ##### Local max #####
  expect_warning(test5 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Local",
    min_max_time = "max"
  ))
  expect_true(tibble::is_tibble(test5))
  expect_equal(ncol(test5), 20)
  expect_equal(nrow(test5), 699)
  expect_equal(unique(test5$institution), "DFCI")
  expect_equal(
    as.character(test5[
      test5$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("44.0789473684211", "Local recurrence")
  )


  ##### Met min/max (are the same) #####
  expect_warning(test6 <- select_unique_ngs(
    data_cohort = cohort_temp$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Metastasis",
    min_max_time = "max"
  ))
  expect_true(tibble::is_tibble(test6))
  expect_equal(ncol(test6), 20)
  expect_equal(nrow(test6), 699)
  expect_equal(unique(test6$institution), "DFCI")
  expect_equal(
    as.character(test6[
      test6$record_id == "GENIE-DFCI-004022",
      c("dx_cpt_rep_mos", "sample_type")
    ]),
    c("31.5131578947368", "Metastasis site unspecified")
  )
})
