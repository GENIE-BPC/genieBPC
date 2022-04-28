test_that("Testing synapse version", {
  # exit if user doesn't have synapser, a log in, or access to data.
  skip_if_not_installed("synapser", minimum_version = NULL)
  skip_if(inherits(try(synapser::synLogin(), silent = TRUE), "try-error"),
          "Not logged into Synapse")
  skip_if(inherits(try(synapser::synGet("syn26948075"), silent = TRUE), "try-error"),
          "Not able to access the data")

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
