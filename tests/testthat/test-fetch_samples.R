test_pull_data <- function(){
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
}
test_pull_data()


test_that("missing input parameters", {
  # cohort name not specified
  expect_error(genieBPC:::fetch_samples())

  # record_id object not specified
  expect_error(genieBPC:::fetch_samples(cohort = "NSCLC"))

  # record_id object specified, but doesn't have variable record_id
  expect_error(genieBPC:::fetch_samples(cohort = "NSCLC",
                             df_record_ids = tibble(cohort = "NSCLC",
                                                    record_id = "123")))
})

test_that("function returns correct number of samples", {
  skip_on_ci()

  # NSCLC #
  ### all samples ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  test1 <- genieBPC:::fetch_samples(
    cohort = "NSCLC", data_synapse = nsclc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test1))
  expect_equal(ncol(test1), 20)
  expect_equal(nrow(test1), 1992)
  expect_equal(length(unique(test1$record_id)), 1849)


  ### Stage IV ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    stage_dx = c("Stage IV"),
    data_synapse = nsclc_data,
    return_summary = FALSE
  )

  test2 <- genieBPC:::fetch_samples(
    cohort = "NSCLC", data_synapse = nsclc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test2))
  expect_equal(ncol(test2), 20)
  expect_equal(nrow(test2), 873)
  expect_equal(length(unique(test2$record_id)), 793)

  ### DFCI only ###
  cohort_temp <- create_analytic_cohort(
    cohort = "NSCLC",
    data_synapse = nsclc_data,
    return_summary = FALSE,
    institution = "DFCI"
  )

  test3 <- genieBPC:::fetch_samples(
    cohort = "NSCLC", data_synapse = nsclc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test3))
  expect_equal(ncol(test3), 20)
  expect_equal(nrow(test3), 736)
  expect_equal(length(unique(test3$record_id)), 699)
  expect_equal(unique(test3$institution), "DFCI")


  ##################################################################


  # CRC #

  ### all samples ###
  cohort_temp <- create_analytic_cohort(
    cohort = "CRC",
    data_synapse = crc_data,
    return_summary = FALSE
  )

  test1 <- genieBPC:::fetch_samples(
    cohort = "CRC", data_synapse = crc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test1))
  expect_equal(ncol(test1), 26)
  expect_equal(nrow(test1), 1566)
  expect_equal(length(unique(test1$record_id)), 1500)


  ### Stage IV ###
  cohort_temp <- create_analytic_cohort(
    cohort = "CRC",
    stage_dx = c("Stage IV"),
    data_synapse = crc_data,
    return_summary = FALSE
  )

  test2 <- genieBPC:::fetch_samples(
    cohort = "CRC", data_synapse = crc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test2))
  expect_equal(ncol(test2), 26)
  expect_equal(nrow(test2), 743)
  expect_equal(length(unique(test2$record_id)), 703)

  ### DFCI only ###
  cohort_temp <- create_analytic_cohort(
    cohort = "CRC",
    data_synapse = crc_data,
    return_summary = FALSE,
    institution = "DFCI"
  )

  test3 <- genieBPC:::fetch_samples(
    cohort = "CRC", data_synapse = crc_data,
    df_record_ids = cohort_temp$cohort_ca_dx
  )
  expect_true(tibble::is_tibble(test3))
  expect_equal(ncol(test3), 26)
  expect_equal(nrow(test3), 577)
  expect_equal(length(unique(test3$record_id)), 570)
  expect_equal(unique(test3$institution), "DFCI")

  expect_error(test4 <- genieBPC:::fetch_samples(
    cohort = "CRC", data_synapse = crc_data,
    df_record_ids = cohort_temp$cohort_ca_dx %>%
      rename(some_name = record_id)
  ))
})
