# pull data and
# return to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(length(if (.is_connected_to_genie()) {
  cohort <- create_analytic_cohort(
    data_synapse = multiple_NSCLC_pull_data_synapse$NSCLC_v2.0,
    stage_dx = c("Stage IV"),
    histology = "Adenocarcinoma",
    regimen_drugs = "Afatinib Dimaleate",
    regimen_type = "Containing"
  )

  plot1 <- drug_regimen_sunburst(
    data_synapse = multiple_NSCLC_pull_data_synapse$NSCLC_v2.0,
    data_cohort = cohort,
    max_n_regimens = 4
  )
} else {
  multiple_NSCLC_pull_data_synapse <- list("a")
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

test_that("Runs for all cohorts", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))

  # NSCLC
  nsclc_v2_0 <- pull_data_synapse("NSCLC", "v2.0-public")
  nsclc_cohort <- create_analytic_cohort(nsclc_v2_0$NSCLC_v2.0)
  testthat::expect_no_error(drug_regimen_sunburst(nsclc_v2_0$NSCLC_v2.0, nsclc_cohort,
                                                  max_n_regimens = 3))

  # CRC
  crc_v2_0 <- pull_data_synapse("CRC", "v2.0-public")
  crc_cohort <- create_analytic_cohort(crc_v2_0$CRC_v2.0)
  testthat::expect_no_error(drug_regimen_sunburst(crc_v2_0$CRC_v2.0,
                                                  crc_cohort,
                                                  max_n_regimens = 3))

  # BrCa
  brca_v1_2 <- pull_data_synapse("BrCa", "v1.2-consortium")
  brca_cohort <- create_analytic_cohort(brca_v1_2$BrCa_v1.2)
  testthat::expect_no_error(drug_regimen_sunburst(brca_v1_2$BrCa_v1.2,
                                                  brca_cohort,
                                                  max_n_regimens = 3))

  # PANC
  panc_v1_2 <- pull_data_synapse("PANC", "v1.2-consortium")
  panc_cohort <- create_analytic_cohort(panc_v1_2$PANC_v1.2)
  testthat::expect_no_error(drug_regimen_sunburst(panc_v1_2$PANC_v1.2,
                                                  panc_cohort,
                                                  max_n_regimens = 3))

  # Prostate
  prostate_v1_2 <- pull_data_synapse(cohort = "Prostate", version = "v1.2-consortium")
  prostate_cohort <- create_analytic_cohort(data_synapse = prostate_v1_2$Prostate_v1.2)
  testthat::expect_no_error(drug_regimen_sunburst(prostate_v1_2$Prostate_v1.2,
                                                  prostate_cohort,
                                                  max_n_regimens = 3))

  # Bladder
  bladder_v1_2 <- pull_data_synapse(cohort = "BLADDER", version = "v1.2-consortium")
  bladder_cohort <- create_analytic_cohort(data_synapse = bladder_v1_2$BLADDER_v1.2)
  testthat::expect_no_error(drug_regimen_sunburst(bladder_v1_2$BLADDER_v1.2,
                                                  bladder_cohort,
                                                  max_n_regimens = 3))
  
  # Renal
  renal_v1_1 <- pull_data_synapse(cohort = "RENAL", version = "v1.1-consortium")
  renal_cohort <- create_analytic_cohort(data_synapse = renal_v1_1$RENAL_v1.1)
  testthat::expect_no_error(drug_regimen_sunburst(renal_v1_1$RENAL_v1.1,
                                                  renal_cohort,
                                                  max_n_regimens = 3))
})

test_that("data_synapse parameter", {
  # missing the data_synapse input parameter
  expect_error(drug_regimen_sunburst())

  # data_synapse input parameter isn't a list
  expect_error(drug_regimen_sunburst(data_synapse = "a"))

  # data_synapse parameter is a list, but isn't the right list
  expect_error(drug_regimen_sunburst(data_synapse = list("a" = "a",
                                                         "b" = "b"),
                                     data_cohort = multiple_NSCLC_pull_data_synapse))
})

test_that("data_cohort parameter", {
  # missing data_cohort parameter
  expect_error(drug_regimen_sunburst(data_synapse = multiple_NSCLC_pull_data_synapse))

  # data_cohort parameter isn't a list
  expect_error(drug_regimen_sunburst(
    data_synapse = multiple_NSCLC_pull_data_synapse,
    data_cohort = "a"
  ))

  # data_cohort parameter is a list, but isn't the right list
  expect_error(drug_regimen_sunburst(
    data_synapse = multiple_NSCLC_pull_data_synapse$NSCLC_v2.0,
    data_cohort = list("a", "b")
  ))
})

test_that("lines of tx specified", {
  skip_if_not(genieBPC:::.is_connected_to_genie())

  # line of therapy isn't specified, select all
  test1a <- drug_regimen_sunburst(
    data_synapse = multiple_NSCLC_pull_data_synapse$NSCLC_v2.0,
    data_cohort = cohort
  )


  # compare to manually inputting the max number
  max_n <- left_join(cohort$cohort_ca_dx,
    multiple_NSCLC_pull_data_synapse$NSCLC_v2.0$ca_drugs,
    by = c("cohort", "record_id", "ca_seq"),
    multiple = "all"
  ) %>%
    drop_na(regimen_drugs) %>%
    count(record_id) %>%
    summarize(n_reg = max(n)) %>%
    pull(n_reg)

  test1b <- drug_regimen_sunburst(
    data_synapse = multiple_NSCLC_pull_data_synapse$NSCLC_v2.0,
    data_cohort = cohort,
    max_n_regimens = max_n
  )

  expect_equal(test1a, test1b)
})
