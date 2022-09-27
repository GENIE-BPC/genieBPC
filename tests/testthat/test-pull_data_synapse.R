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

  expect_error(pull_data_synapse(
    cohort = "nsclc",
    version = "v1.1-consortium"
  ), "*")
})

test_that("test `version` argument specification", {
  skip_if_not(.is_connected_to_genie())

  # no version specified
  expect_error(
    pull_data_synapse(
      cohort = "NSCLC",
      version = NULL
    ),
    "Version needs to be specified.*"
  )

  # incorrect arg formatting
  expect_error(pull_data_synapse(
    cohort = "NSCLC",
    version = "1.1"
  ), "*")

  # more versions than cohorts
  expect_error(
    pull_data_synapse(
      cohort = "NSCLC",
      version = c(
        "v1.1-consortium",
        "v2.1-consortium"
      )
    ),
    "*You have selected"
  )


  # mismatch version-cohort
  expect_error(
    pull_data_synapse(
      cohort = "BrCa",
      version = c("v2.1-consortium")
    ),
    "You have selected a version that is not available for this cohort*"
  )
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
  expect_equal(row_length, c(
    1849, 1874, 810, 4032, 35113, 8329,
    24950, 2026, 821, 930, 17574
  ))
})

test_that("Test NA conversion", {
  skip_if_not(.is_connected_to_genie())
  # test NSCLC
  nsclc_cohort <- pull_data_synapse(cohort = c("NSCLC", "NSCLC", "NSCLC"),
                                    version = c("v1.1-consortium",
                                                "v2.1-consortium",
                                                "v2.0-public"))
  # NSCLC 1.1-consortium
  lung_nomissing_1.1 <- sum(sapply(nsclc_cohort$NSCLC_v1.1, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(lung_nomissing_1.1, length(nsclc_cohort$NSCLC_v1.1))

  # NSCLC 2.1-consortium
  lung_nomissing_2.1 <- sum(sapply(nsclc_cohort$NSCLC_v2.1, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(lung_nomissing_2.1, length(nsclc_cohort$NSCLC_v2.1))

  # NSCLC 2.0-public
  lung_nomissing_2.0<- sum(sapply(nsclc_cohort$NSCLC_v2.0, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(lung_nomissing_2.0, length(nsclc_cohort$NSCLC_v2.0))

  #### test CRC
  crc_cohort <- pull_data_synapse(cohort = c("CRC", "CRC"),
                                    version = c("v1.1-consortium",
                                                "v1.2-consortium"))
  # CRC 1.1-consortium
  crc_nomissing_1.1 <- sum(sapply(crc_cohort$CRC_v1.1, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(crc_nomissing_1.1, length(crc_cohort$CRC_v1.1))

  # CRC 1.2-consortium
  crc_nomissing_1.2 <- sum(sapply(crc_cohort$CRC_v1.2, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(crc_nomissing_1.2, length(crc_cohort$CRC_v1.2))

  #### test BrCa
  brca_cohort <- pull_data_synapse(cohort = c("BrCa"),
                                  version = c("v1.1-consortium"))
  # CRC 1.1-consortium
  brca_nomissing_1.1 <- sum(sapply(brca_cohort$BrCa_v1.1, function(x) {
    any(x == "", na.rm = TRUE) == FALSE
  }))

  expect_equal(brca_nomissing_1.1, length(brca_cohort$BrCa_v1.1))
})
