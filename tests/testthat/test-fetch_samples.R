# # run here to avoid having to run within each test
# nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
# crc_data <- pull_data_synapse(c("CRC"), version = "1.1")
#
# test_that("function returns correct number of samples", {
#
#   # NSCLC #
#
#   ### all samples ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = FALSE
#   )
#
#   test1 <- fetch_samples(
#     cohort = "NSCLC", data_synapse = nsclc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test1))
#   expect_equal(ncol(test1), 20)
#   expect_equal(nrow(test1), 1992)
#   expect_equal(length(unique(test1$record_id)), 1849)
#
#
#   ### Stage IV ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "NSCLC",
#     stage_dx = c("Stage IV"),
#     data_synapse = nsclc_data,
#     return_summary = FALSE
#   )
#
#   test2 <- fetch_samples(
#     cohort = "NSCLC", data_synapse = nsclc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test2))
#   expect_equal(ncol(test2), 20)
#   expect_equal(nrow(test2), 873)
#   expect_equal(length(unique(test2$record_id)), 793)
#
#   ### DFCI only ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = FALSE,
#     institution = "DFCI"
#   )
#
#   test3 <- fetch_samples(
#     cohort = "NSCLC", data_synapse = nsclc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test3))
#   expect_equal(ncol(test3), 20)
#   expect_equal(nrow(test3), 736)
#   expect_equal(length(unique(test3$record_id)), 699)
#   expect_equal(unique(test3$institution), "DFCI")
#
#
#   ##################################################################
#
#
#   # CRC #
#
#   ### all samples ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "CRC",
#     data_synapse = crc_data,
#     return_summary = FALSE
#   )
#
#   test1 <- fetch_samples(
#     cohort = "CRC", data_synapse = crc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test1))
#   expect_equal(ncol(test1), 26)
#   expect_equal(nrow(test1), 1566)
#   expect_equal(length(unique(test1$record_id)), 1500)
#
#
#   ### Stage IV ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "CRC",
#     stage_dx = c("Stage IV"),
#     data_synapse = crc_data,
#     return_summary = FALSE
#   )
#
#   test2 <- fetch_samples(
#     cohort = "CRC", data_synapse = crc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test2))
#   expect_equal(ncol(test2), 26)
#   expect_equal(nrow(test2), 743)
#   expect_equal(length(unique(test2$record_id)), 703)
#
#   ### DFCI only ###
#   cohort_temp <- create_analytic_cohort(
#     cohort = "CRC",
#     data_synapse = crc_data,
#     return_summary = FALSE,
#     institution = "DFCI"
#   )
#
#   test3 <- fetch_samples(
#     cohort = "CRC", data_synapse = crc_data,
#     df_record_ids = cohort_temp$cohort_ca_dx
#   )
#   expect_true(tibble::is_tibble(test3))
#   expect_equal(ncol(test3), 26)
#   expect_equal(nrow(test3), 577)
#   expect_equal(length(unique(test3$record_id)), 570)
#   expect_equal(unique(test3$institution), "DFCI")
# })
