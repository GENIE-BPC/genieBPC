test_that("Testing synapse version", {
  # exit if user doesn't have synapser, a log in, or access to data.
  testthat::skip_if_not(.is_connected_to_genie())

  expect_equal(
    class(synapse_version(most_recent = FALSE)),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    class(synapse_version(most_recent = TRUE)),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    nrow(synapse_version(most_recent = TRUE)) <
      nrow(synapse_version(most_recent = FALSE)),
    TRUE
  )
})

test_that("Test most_recent argument", {
  expect_error(synapse_version(most_recent = "ABC"))
  expect_error(synapse_version(most_recent = 123))
})

test_that("Test cohort argument", {
  # if no cohort specified, all cohorts returned
  expect_equal(nrow(synapse_version()),
               nrow(synapse_tables %>% distinct(cohort, version)))

  # if cohort is specified, only that cohort is returned
  # run for each cohort
  imap(
    synapse_tables %>% split(.$cohort),
    ~ expect_equal(
      nrow(synapse_version(cohort = .y)),
      nrow(synapse_tables %>%
             distinct(cohort, version) %>%
             filter(cohort == .y))
    )
  )

  # if multiple cohorts are specified, each cohort is returned
  expect_equal(
    nrow(synapse_version(cohort = c("NSCLC", "CRC"))),
    nrow(synapse_tables %>%
           distinct(cohort, version) %>%
           filter(cohort %in% c("NSCLC", "CRC")))
  )
})

test_that("Test `cohort` argument specification casing", {
  # expect lower case cohort to work
  expect_equal(synapse_version(cohort = "NSCLC"),
               synapse_version(cohort = "nsclc"))

  expect_equal(synapse_version(cohort = "CRC"),
               synapse_version(cohort = "crC"))

  expect_equal(synapse_version(cohort = "BrCa"),
               synapse_version(cohort = "BRCA"))

  expect_equal(synapse_version(cohort = "PANC"),
               synapse_version(cohort = "Pancreas"))

  expect_equal(synapse_version(cohort = "PANC"),
               synapse_version(cohort = "Panc"))

  expect_equal(synapse_version(cohort = "Prostate"),
               synapse_version(cohort = "PROState"))

  expect_equal(synapse_version(cohort = "BLADDER"),
               synapse_version(cohort = "Bladder"))
})

test_that("Test most_recent = TRUE", {
  # expect 1 row per cohort
  expect_equal(synapse_version(most_recent = TRUE) %>%
                 nrow(),
               synapse_version(most_recent = TRUE) %>%
                 dplyr::distinct(cohort) %>%
                 nrow())
})
