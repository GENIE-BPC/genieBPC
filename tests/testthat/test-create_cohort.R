# adding this one fake test to initialize the unit testing framework

# test that three datasets are returned from create_cohort
test_that("correct number of datasets returned from create cohort", {
  nsclc_crc_data <- pull_data_synapse(c("NSCLC"))
  list2env(nsclc_crc_data, envir = globalenv())

  test1 <- create_cohort(
    cohort = "NSCLC",
    return_summary = FALSE
  )

  expect_equal(length(test1), 3)
})

test_that("correct cohort returned from create cohort", {
  nsclc_crc_data <- pull_data_synapse(c("NSCLC"))
  list2env(nsclc_crc_data, envir = globalenv())

  test1 <- create_cohort(
    cohort = "NSCLC",
    return_summary = FALSE
  )

  expect_equal(unique(test1$cohort_ca_dx$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_ca_drugs$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_cpt$cohort), "NSCLC")
})
