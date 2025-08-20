
# Tests - No GENIE Access Required ---------------------------------------------

test_that("No specifications- runs with no error", {
  expect_error(create_analytic_cohort(
    data_synapse = genieBPC::nsclc_test_data
  ), NA)
})

test_that("pull data synapse object is missing", {
  expect_error(create_analytic_cohort())
})


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

