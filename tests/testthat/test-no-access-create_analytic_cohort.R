
# Tests - No GENIE Access Required ---------------------------------------------
test_that("data_synapse - no argument passed", {

  # a non-existent data_synapse is specified
  expect_error(
    create_analytic_cohort(
      data_synapse = data_releases_pull_data$TEST_NONEXIST))

  expect_error(
    create_analytic_cohort(
      data_synapse = data_releases_pull_data[[1]]$TEST_NONEXIST))

  expect_error(
    create_analytic_cohort(
      data_synapse = data_releases_pull_data$TEST_NONEXIST))
})

test_that("No specifications- runs with no error", {
  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data
  ), NA)
})

test_that("pull data synapse object is missing", {
  expect_error(create_analytic_cohort())
})

# test_that("Institution- argument check", {
#   expect_error(msk <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "MSK"
#   ), NA)
#
#   expect_equal("MSK", unique(msk[[1]]$institution))
#
#   expect_error(dfci <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "DFCI"
#   ), NA)
#
#   expect_equal("DFCI", unique(dfci[[1]]$institution))
#
#   expect_error(vicc <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "VICC"
#   ), NA)
#
#   expect_equal("VICC", unique(vicc[[1]]$institution))
#
#   expect_error(uhn <- create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "UHN"
#   ), NA)
#
#   expect_equal("UHN", unique(uhn[[1]]$institution))
#
#   expect_error(create_analytic_cohort(
#     data_synapse = genieBPC::nsclc_test_data,
#     institution = "non-existant"
#   ), "The specified institution*")
# })


# * Check Arguments -----------

# ** Institution ------
test_that("institution - argument check", {
  valid_institutions <- c("MSK", "DFCI", "VICC", "UHN")

  for (inst in valid_institutions) {
    expect_error(
      cohort <- create_analytic_cohort(
        data_synapse = genieBPC::nsclc_test_data,
        institution = inst
      ),
      NA
    )
    expect_equal(inst, unique(cohort[[1]]$institution))
  }

  expect_error(
    create_analytic_cohort(
      data_synapse = genieBPC::nsclc_test_data,
      institution = "non-existant"
    ),
    "The specified institution*"
  )
})

# ** Stage ------
test_that("stage_dx - argument check", {

  valid_stages <- c("Stage I", "Stage II", "Stage III", "Stage IV", "staGe IV")

  for (stage in valid_stages) {
    expect_error(
      create_analytic_cohort(
        data_synapse = genieBPC::nsclc_test_data,
        stage_dx = stage), NA)
  }
})

test_that("stage_dx - two args passed", {

  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = c("Stage I", "Stage IV")
  ), NA)
})

test_that("stage_dx - no args passed", {
  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data,
    stage_dx = "none"
  ), "*")
})
