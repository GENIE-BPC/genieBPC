# test_that("Missing cohort parameter", {
#   expect_error(pull_data_synapse())
# })
#
# test_that("Test class and length of list for NSCLC", {
#
#   skip_if_not(.check_synapse_login())
#
#   # run at top to avoid having to run within each test
#   nsclc_data <- pull_data_synapse("NSCLC", version = "1.1-consortium")
#   crc_data <- pull_data_synapse(c("CRC"), version = "1.1-consortium")
#
#   objs <- list("nsclc_data" = nsclc_data,
#                "crc_data" = crc_data)
#
#   list2env(objs, envir = .GlobalEnv)
#
#   expect_equal(length(nsclc_data), 11)
#   expect_equal(class(nsclc_data), "list")
# })
# #
#
# test_that("Test class length of list for CRC", {
#   # exit if user doesn't have synapser, a log in, or access to data.
#   skip_if_not_installed("synapser", minimum_version = NULL)
#   skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
#           "Not logged into Synapse")
#   skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
#           "Not able to access the data")
#
#   expect_equal(length(crc_data), 12)
#   expect_equal(class(crc_data), "list")
# })
#
# test_that("Case sensitivity of cohort", {
#   # exit if user doesn't have synapser, a log in, or access to data.
#   skip_if_not_installed("synapser", minimum_version = NULL)
#   skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
#           "Not logged into Synapse")
#   skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
#           "Not able to access the data")
#
#   lung1 <- pull_data_synapse("NSCLC", version = "1.1-consortium")
#   lung2 <- pull_data_synapse("nsclc", version = "1.1-consortium")
#   expect_equal(lung1, lung2)
# })
#
# test_that("Version not specified", {
#   expect_error(pull_data_synapse(cohort = "NSCLC"))
# })
#
# test_that("Misspecified cohort or version", {
#   expect_error(pull_data_synapse(c("NSCLC"), version = "0.1"))
#   expect_error(pull_data_synapse(c("lung"), version = "1.1-consortium"))
# })
#
# test_that("Number of columns and rows for each NSCLC dataset", {
#   # exit if user doesn't have synapser, a log in, or access to data.
#   skip_if_not_installed("synapser", minimum_version = NULL)
#   skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
#           "Not logged into Synapse")
#   skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
#           "Not able to access the data")
#
#   col_length <- vapply(nsclc_data, length, FUN.VALUE = numeric(1))
#   row_length <- vapply(nsclc_data, nrow, FUN.VALUE = numeric(1))
#   names(col_length) <- NULL
#   names(row_length) <- NULL
#   expect_equal(col_length, c(33, 110, 83, 114, 195, 42, 11, 19, 1782, 9, 54))
#   expect_equal(row_length, c(1849, 1874, 810, 4032, 8329,
#                              35113, 24950, 2026, 930, 821, 17574))
# })
#
# test_that("Number of columns and rows for each CRC dataset", {
#   # exit if user doesn't have synapser, a log in, or access to data.
#   skip_if_not_installed("synapser", minimum_version = NULL)
#   skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
#           "Not logged into Synapse")
#   skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
#           "Not able to access the data")
#
#   col_length <- vapply(crc_data, length, FUN.VALUE = numeric(1))
#   row_length <- vapply(crc_data, nrow, FUN.VALUE = numeric(1))
#   names(col_length) <- NULL
#   names(row_length) <- NULL
#   expect_equal(col_length, c(37, 111, 87, 102, 340, 42,
#                              11, 12, 25, 1505, 9, 54))
#   expect_equal(row_length, c(1500, 1510, 353, 5459, 7216,
#                              26500, 28467, 24708, 1576, 930, 406, 23445))
# })
#
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
