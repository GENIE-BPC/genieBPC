#
# nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
#
# cohort <- create_analytic_cohort(cohort = "NSCLC", data_synapse = nsclc_data,
#   stage_dx = c("Stage IV"),
#   histology = "Adenocarcinoma",
#   regimen_drugs = "Afatinib Dimaleate",
#   regimen_type = "Containing"
#   )
#
#
# plot1 <- drug_regimen_sunburst( data_synapse = nsclc_data, data_cohort = cohort,
#                        max_n_regimens = 4)
#
#
# test_that("Test class and length of list for sunburst plot", {
#
#   expect_equal(length(plot1), 2)
#   expect_equal(class(plot1), "list")
# })
#
#
# test_that("Test class and length of list for elements of sunburst data frame", {
#
#   expect_equal(length(plot1$treatment_history), 2)
#   expect_equal(class(plot1$treatment_history), c("tbl_df", "tbl" , "data.frame"))
# })
#
#
# test_that("Test class and length of list for elements of sunburst plotly element", {
#
#   expect_equal(length(plot1$sunburst_plot), 8)
#   expect_equal(class(plot1$sunburst_plot), c("sunburst","htmlwidget"))
# })
#
#
# test_that("Test something is returned", {
#   expect_error(plot1,NA)
# })
#
#
#
#
#
