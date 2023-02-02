# pull data and
# return to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(length(if (.is_connected_to_genie()) {
  nsclc_data <- pull_data_synapse("NSCLC",
    version = "v1.1-consortium"
  )

  cohort <- create_analytic_cohort(
    data_synapse = nsclc_data$NSCLC_v1.1,
    stage_dx = c("Stage IV"),
    histology = "Adenocarcinoma",
    regimen_drugs = "Afatinib Dimaleate",
    regimen_type = "Containing"
  )

  plot1 <- drug_regimen_sunburst(
    data_synapse = nsclc_data$NSCLC_v1.1,
    data_cohort = cohort,
    max_n_regimens = 4
  )
} else {
  nsclc_data <- list("a")
}) > 0)


test_that("Test class and length of list for sunburst plot", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  expect_equal(length(plot1), 2)
  expect_equal(class(plot1), "list")
})


test_that("Test class and length of list for elements of sunburst data frame", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  expect_equal(length(plot1$treatment_history), 2)
  expect_equal(
    class(plot1$treatment_history),
    c("tbl_df", "tbl", "data.frame")
  )
})


test_that("Test class and length of list for elements
          of sunburst plotly element", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  expect_equal(length(plot1$sunburst_plot), 8)
  expect_equal(class(plot1$sunburst_plot), c("sunburst", "htmlwidget"))
})


test_that("Test something is returned", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  expect_error(plot1, NA)
})

test_that("data_synapse parameter", {
  # missing the data_synapse input parameter
  expect_error(drug_regimen_sunburst())

  # data_synapse input parameter isn't a list
  expect_error(drug_regimen_sunburst(data_synapse = "a"))

  # data_synapse parameter is a list, but isn't the right list
  expect_error(drug_regimen_sunburst(data_synapse = list("a" = "a",
                                                         "b" = "b"),
                                     data_cohort = nsclc_data))
})

test_that("data_cohort parameter", {
  # missing data_cohort parameter
  expect_error(drug_regimen_sunburst(data_synapse = nsclc_data))

  # data_cohort parameter isn't a list
  expect_error(drug_regimen_sunburst(
    data_synapse = nsclc_data,
    data_cohort = "a"
  ))

  # data_cohort parameter is a list, but isn't the right list
  expect_error(drug_regimen_sunburst(
    data_synapse = nsclc_data$NSCLC_v1.1,
    data_cohort = list("a", "b")
  ))
})

test_that("lines of tx specified", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  # line of therapy isn't specified, select all
  test1a <- drug_regimen_sunburst(
    data_synapse = nsclc_data$NSCLC_v1.1,
    data_cohort = cohort
  )


  # compare to manually inputting the max number
  max_n <- left_join(cohort$cohort_ca_dx,
    nsclc_data$NSCLC_v1.1$ca_drugs,
    by = c("cohort", "record_id", "ca_seq"),
    multiple = "all"
  ) %>%
    drop_na(regimen_drugs) %>%
    count(record_id) %>%
    summarize(n_reg = max(n)) %>%
    pull(n_reg)

  test1b <- drug_regimen_sunburst(
    data_synapse = nsclc_data$NSCLC_v1.1,
    data_cohort = cohort,
    max_n_regimens = max_n
  )

  expect_equal(test1a, test1b)
})
