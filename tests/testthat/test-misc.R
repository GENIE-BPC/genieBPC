

# Not Consortium Specific Check -------------------------------------------


test_that("Test no error with full access PAT", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))
  expect_no_error(check_genie_access(pat = Sys.getenv("SYNAPSE_PAT")))
})

test_that("Test no error with Public access PAT, not consortium specific check", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))
  expect_no_error(check_genie_access(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))
})

test_that("Test no error when access but terms not checked", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS"))
  expect_no_error(check_genie_access(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")))
})


# Consortium Specific Check -------------------------------------------

test_that("Test no error with full access PAT and consortium check", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))
  expect_no_error(
    check_genie_access(pat = Sys.getenv("SYNAPSE_PAT"),
                       check_consortium_access = TRUE))
})

test_that("Test error with Public access only PAT, but consortium specific check", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))
  expect_error(
    check_genie_access(
      pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"),
      check_consortium_access = TRUE))
})

test_that("Test no error when access but terms not checked", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS"))
  expect_error(
    check_genie_access(
      pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS"),
      check_consortium_access = TRUE))
})

