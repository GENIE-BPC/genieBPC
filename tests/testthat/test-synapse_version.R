test_that("Testing synapse version", {
  # exit if user doesn't have synapser, a log in, or access to data.
  testthat::skip_if_not(check_genie_access())

  expect_equal(
    class(synapse_version(FALSE)),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    class(synapse_version(TRUE)),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(nrow(synapse_version(TRUE)) < nrow(synapse_version(FALSE)),
               TRUE)
})

test_that("Test most_recent argument", {
  expect_error(synapse_version(most_recent = "ABC"))
  expect_error(synapse_version(most_recent = 123))
})
