test_that("Missing cohort parameter", {
  expect_error(pull_data_synapse())
})

test_that("Test class and length of list for NSCLC", {

  skip_if_not(.is_connected_to_genie())

  nsclc_data <- pull_data_synapse("NSCLC", version = "v1.1-consortium")
  crc_data <- pull_data_synapse(c("CRC"), version = "v1.1-consortium")

  expect_equal(length(nsclc_data[[1]]), 11)
  expect_equal(length(crc_data[[1]]), 12)
  expect_equal(class(nsclc_data[1]), "list")
})


test_that("test `cohort` argument specification", {

  skip_if_not(.is_connected_to_genie())

  expect_error(pull_data_synapse(cohort = "nsclc",
                                 version = "v1.1-consortium"), "*")


})

test_that("test `version` argument specification", {

  skip_if_not(.is_connected_to_genie())

  # no version specified
  expect_error(pull_data_synapse(cohort = "NSCLC",
                                 version = NULL), "Version needs to be specified.*")

  # incorrect arg formatting
  expect_error(pull_data_synapse(cohort = "NSCLC",
                                 version = "1.1"), "*")

  # more versions than cohorts
  expect_error(pull_data_synapse(cohort = "NSCLC",
                                 version = c("v1.1-consortium", "v2.1-consortium")), "*You have selected")


  # mismatch version-cohort
  expect_error(pull_data_synapse(cohort = "BrCa",
                                 version = c("v2.1-consortium")), "You have selected a version that is not available for this cohort*")
})



test_that("Number of columns and rows for each NSCLC dataset", {

  skip_if_not(.is_connected_to_genie())
  nsclc_data <- pull_data_synapse("NSCLC", version = "v1.1-consortium")
  nsclc_data <- nsclc_data$NSCLC_v1.1

  col_length <- vapply(nsclc_data, length, FUN.VALUE = numeric(1))
  row_length <- vapply(nsclc_data, nrow, FUN.VALUE = numeric(1))
  names(col_length) <- NULL
  names(row_length) <- NULL

  expect_equal(col_length, c(33, 110, 83, 114, 42, 195, 11, 19, 9, 1782, 54))
  expect_equal(row_length, c(1849, 1874, 810, 4032, 35113, 8329,
                             24950, 2026, 821, 930, 17574))
})

# May want to update the test below:

# test_that("Test synget equals pulldata synapse", {
#   # exit if user doesn't have synapser, a log in, or access to data.
#   skip_if_not_installed("synapser", minimum_version = NULL)
#   skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
#           "Not logged into Synapse")
#   skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
#           "Not able to access the data")
#
#   ptchar_nsclc_synget <- read.csv(
#     synapser::synGet("syn22418979")$path) # version 1.1-consortium
#   ptchar_nsclc_pulldata <- pull_data_synapse("NSCLC", "1.1-consortium")[[1]]
#   expect_equal(ptchar_nsclc_synget, ptchar_nsclc_pulldata)
# })
#
# test_that("More versions than cancer cohorts selected", {
#   expect_error(pull_data_synapse(cohort = c("NSCLC", "CRC"),
#                                    version = c("1.1-consortium", "1.1-consortium", "2.1-consortium")))
# })
