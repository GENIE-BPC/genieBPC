test_that("Invalid function parameter", {
  expect_false(.is_connected_to_genie("GENIE"))
})

test_that("Nothing supplied, see if PAT is found from environment", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_message(
    set_synapse_credentials(), "You are now connected to 'Synapse' with your Personal*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_message(
    set_synapse_credentials(pat = NULL), "You are now connected to 'Synapse' with your Personal*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_error(
    set_synapse_credentials(pat = ""), "*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  expect_error(
    set_synapse_credentials(pat = " "), "*"
  )
})
