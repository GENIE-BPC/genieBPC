test_that("Invalid function parameter", {
  expect_error(check_synapse_login("GENIE"))
})

test_that("Test Synapse log in", {
  synapser::synLogout()
  expect_error(check_synapse_login())
})
