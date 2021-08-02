# run here to avoid having to run within each test
nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
crc_data <- pull_data_synapse(c("CRC"), version = "1.1")


test_that("function returns unique sample for each record", {

  # NSCLC #

  ### all samples ###
  record_ids <- nsclc_data$ca_dx_index_NSCLC
  ca_drugs <- nsclc_data$ca_drugs_NSCLC
  regimen_drugs <- unique(ca_drugs$regimen_drugs)
  test1 <- treatment_history(ids = record_ids, ca_drugs = ca_drugs,
                             regimen_drugs = regimen_drugs, lines_keep = NULL)
  expect_equal(nrow(test1$treat_hist),1284)
  expect_equal(typeof(test1$p_dist), "list")


  ### Stage IV ###
  record_ids <- nsclc_data$ca_dx_index_NSCLC[nsclc_data$ca_dx_index_NSCLC$stage_dx == "Stage IV",]
  ca_drugs <- nsclc_data$ca_drugs_NSCLC
  regimen_drugs <- unique(ca_drugs$regimen_drugs)
  test2 <- treatment_history(ids = record_ids, ca_drugs = ca_drugs,
                             regimen_drugs = regimen_drugs, lines_keep = NULL)
  expect_equal(nrow(test2$treat_hist),691)
  expect_equal(typeof(test2$p_dist), "list")

  ### line 1-3 ###
  record_ids <- nsclc_data$ca_dx_index_NSCLC[nsclc_data$ca_dx_index_NSCLC$stage_dx == "Stage IV",]
  ca_drugs <- nsclc_data$ca_drugs_NSCLC
  regimen_drugs <- unique(ca_drugs$regimen_drugs)
  test3 <- treatment_history(ids = record_ids, ca_drugs = ca_drugs,
                             regimen_drugs = regimen_drugs, lines_keep = 1:3)
  expect_equal(nrow(test3$treat_hist),691)
  expect_equal(typeof(test3$p_dist), "list")
}
)
