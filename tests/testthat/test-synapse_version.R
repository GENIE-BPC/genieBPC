test_that("Testing synapse version", {
  genieBPC:::check_synapse_login()

  expect_equal(class(synapse_version(FALSE)),
               c("grouped_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(class(synapse_version(TRUE)),
               c("grouped_df", "tbl_df", "tbl", "data.frame"))

  expect_equal(synapse_version(TRUE) %>%
                 dplyr::count(cohort) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct(n) %>%
                 as.data.frame(), data.frame(n = 1))
})

test_that("Test most_recent argument", {
  expect_error(synapse_version(most_recent = "ABC"))
  expect_error(synapse_version(most_recent = 123))
})
