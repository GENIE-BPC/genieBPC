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
