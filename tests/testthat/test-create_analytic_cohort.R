# # run here to avoid having to run within each test
# nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")
# crc_data <- pull_data_synapse(c("CRC"), version = "1.1")
#
# # test that a list of three or seven datasets are returned from create_analytic_cohort
# test_that("correct number of objects returned from create cohort", {
#   nsclc_data <- pull_data_synapse(c("NSCLC"), version = "1.1")
#
#   test1 <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = FALSE
#   )
#
#   expect_equal(length(test1), 3)
#   expect_equal(class(test1), "list")
#
#   test2 <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = TRUE
#   )
#
#   expect_equal(length(test2), 7)
#   expect_equal(class(test2), "list")
# })
#
# test_that("correct cohort returned from create cohort", {
#   test1 <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = FALSE
#   )
#
#   expect_equal(unique(test1$cohort_ca_dx$cohort), "NSCLC")
#   expect_equal(unique(test1$cohort_ca_drugs$cohort), "NSCLC")
#   expect_equal(unique(test1$cohort_cpt$cohort), "NSCLC")
#
#   # check CRC
#   test2 <- create_analytic_cohort(
#     cohort = "CRC",
#     data_synapse = crc_data,
#     return_summary = FALSE
#   )
#
#   expect_equal(unique(test2$cohort_ca_dx$cohort), "CRC")
#   expect_equal(unique(test2$cohort_ca_drugs$cohort), "CRC")
#   expect_equal(unique(test2$cohort_cpt$cohort), "CRC")
# })
#
# test_that("cohort and data_synapse", {
#   # no diagnosis criteria specified
#   # expect that the first index cancer is returned without any other
#   # incl criteria
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data
#   )
#
#   test_1b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup()
#
#   expect_equal(test_1a$cohort_ca_dx, test_1b)
#
#   # errors if non-existent cohort or data_synapse are specified
#   # a non-existent cohort is specified
#   expect_error(create_analytic_cohort(
#     cohort = "made up cohort"
#   ))
#
#   # a non-existent data_synapse is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = crc_data
#   ))
# })
#
# test_that("index_ca_seq", {
#   # first and second index cancer is specified
#   # if patient only has 1 index cancer, it should be returned
#   # if patient has 2+ index cancers, the first two should be returned
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     index_ca_seq = c(1, 2),
#     return_summary = TRUE
#   )
#
#   test_1b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     arrange(cohort, record_id, ca_seq) %>%
#     mutate(index_ca_seq = 1:n()) %>%
#     ungroup() %>%
#     filter(index_ca_seq %in% c(1, 2)) %>%
#     select(-index_ca_seq)
#
#   expect_equal(test_1a$cohort_ca_dx, test_1b)
#
#   # an index cancer # that doesn't exist in the data is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     index_ca_seq = 10
#   ))
#
#   ## index cancer #s in cohort_ca_drugs and cohort_cpt match those in cohort_ca_dx
#   test2a <- create_analytic_cohort(
#     cohort = "CRC",
#     data_synapse = crc_data,
#     index_ca_seq = c(1, 2)
#   )
#
#   expect_equal(
#     test2a$cohort_ca_dx %>%
#       select(record_id, ca_seq) %>%
#       arrange(record_id, ca_seq),
#     test2a$cohort_ca_drugs %>%
#       select(record_id, ca_seq) %>%
#       distinct() %>%
#       arrange(record_id, ca_seq)
#   )
#
#   expect_equal(
#     test2a$cohort_ca_dx %>%
#       select(record_id, ca_seq) %>%
#       arrange(record_id, ca_seq),
#     test2a$cohort_cpt %>%
#       select(record_id, ca_seq) %>%
#       distinct() %>%
#       arrange(record_id, ca_seq)
#   )
# })
#
# test_that("institution", {
#   # institution is specified and correct institution is returned
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     institution = "dfci"
#   )
#
#   test_1b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(institution == "DFCI")
#
#   expect_equal(test_1a$cohort_ca_dx, test_1b)
#
#   # multiple institutions specified
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     institution = c("dfci", "msk")
#   )
#
#   test_2b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(institution %in% c("MSK", "DFCI"))
#
#   expect_equal(test_2a$cohort_ca_dx, test_2b)
#
#   # a non-existent institution is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     institution = "uDFCI"
#   ))
#
#
#   expect_error(create_analytic_cohort(
#     cohort = "CRC",
#     data_synapse = crc_data,
#     institution = "UHN"
#   ))
# })
#
# test_that("stage_dx", {
#   # stage dx is specified and correct stage is returned
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     stage_dx = "stage ii"
#   )
#
#   test_1b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(stage_dx == "Stage II")
#
#   expect_equal(test_1a$cohort_ca_dx, test_1b)
#
#   # multiple stage values are specified
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     stage_dx = c("Stage I", "stage ii")
#   )
#
#   test_2b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(stage_dx %in% c("Stage I", "Stage II"))
#
#   expect_equal(test_2a$cohort_ca_dx, test_2b)
#
#   # non-existent stage is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     stage_dx = "3A"
#   ))
# })
#
# test_that("histology", {
#   # histology is specified and correct histology is returned
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     histology = "adenocarcinoma"
#   )
#
#   test_1b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(histology == "Adenocarcinoma")
#
#   expect_equal(test_1a$cohort_ca_dx, test_1b)
#
#   # multiple histologies are specified and returned
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     histology = c("adenocarcinoma", "squamous cell")
#   )
#
#   test_2b <- nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(cohort, record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup() %>%
#     filter(histology %in% c("Adenocarcinoma", "Squamous cell"))
#
#   expect_equal(test_2a$cohort_ca_dx, test_2b)
#
#   # a non-existent histology is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     histology = "squamous_adeno"
#   ))
# })
#
# test_that("stage", {
#   # a non-existent stage is specified
#   expect_error(create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     stage_dx = "stage 12"
#   ))
# })
#
# test_that("no regimen specified", {
#   # all regimens are returned
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     return_summary = FALSE
#   )
#
#   # should match all regimens given for a patients first index cancer
#   test_1b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup(),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c("cohort", "record_id", "institution", "ca_seq")
#   )
#
#   expect_equal(test_1a$cohort_ca_drugs, test_1b)
# })
#
# test_that("drug regimen specified, order not specified", {
#   # one drug regimen specified, but order not specified
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium")
#   )
#
#   # expect all times that drug was received (for the first index ca)
#   # to be returned
#   test_1b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup(),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "institution",
#     "ca_seq"
#   )
#   ) %>%
#     filter(regimen_drugs == c("Carboplatin, Pemetrexed Disodium"))
#
#   expect_equal(test_1a$cohort_ca_drugs, test_1b)
#
#   # also expect only diagnoses to patients who received this drug regimen
#   # to be returned
#   ### have to come back here
#
#   # one drug regimen specified with drugs out of ABC order and in mixed case
#   # regimen order not specified
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Pemetrexed DISODIUM, carboplatin")
#   )
#
#   # expect all times that drug was received (for the first index ca)
#   # to be returned
#   # same as above
#
#   expect_equal(test_2a$cohort_ca_drugs, test_1b)
#
#   # multiple drug regimens specified, but order not specified
#   test_3a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab")
#   )
#
#   # expect all times that drug was received (for the first index ca)
#   # to be returned
#   test_3b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup(),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "institution",
#     "ca_seq"
#   )
#   ) %>%
#     filter(regimen_drugs %in% c(
#       "Carboplatin, Pemetrexed Disodium",
#       "Nivolumab"
#     ))
#
#   expect_equal(test_3a$cohort_ca_drugs, test_3b)
#
#   # multiple drug regimens specified, regimen_type = containing
#   test_4a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin", "Nivolumab"),
#     regimen_type = "containING"
#   )
#
#   # expect all times that drug was received (for the first index ca)
#   # to be returned
#   test_4b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)) %>%
#     ungroup(),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id",
#     "institution", "ca_seq"
#   )
#   ) %>%
#     filter(grepl("Carboplatin", regimen_drugs) |
#       grepl("Nivolumab", regimen_drugs))
#
#   expect_equal(test_4a$cohort_ca_drugs, test_4b)
# })
#
# test_that("drug regimen specified, order specified to be within cancer", {
#   # all patients whose first drug after diagnosis was carbo pem
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
#     regimen_type = "Exact",
#     regimen_order = 1,
#     regimen_order_type = "within cancer"
#   )
#
#   # compare to data
#   test_1b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c("cohort", "record_id", "ca_seq", "institution")
#   ) %>%
#     group_by(record_id) %>%
#     slice(which.min(regimen_number)) %>%
#     ungroup() %>%
#     filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium")
#
#   expect_equal(test_1a$cohort_ca_drugs, test_1b)
#
#   # second regimen after diagnosis was carbo pem
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
#     regimen_type = "Exact",
#     regimen_order = 2,
#     regimen_order_type = "within cancer"
#   )
#
#   # compare to data
#   test_2b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
#     filter(new_reg_number == 2) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_2a$cohort_ca_drugs, test_2b)
#
#   # first AND/OR second regimen after diagnosis was carbo pem
#   test_3a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
#     regimen_type = "Exact",
#     regimen_order = c(1, 2),
#     regimen_order_type = "within cancer"
#   )
#
#   # compare to data
#   test_3b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
#     filter(new_reg_number %in% c(1, 2)) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_3a$cohort_ca_drugs, test_3b)
#
#   # first AND/OR second regimen after diagnosis was carbo pem
#   # regimen_type = containing rather than default of exact
#   test_4a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
#     regimen_type = "containing",
#     regimen_order = c(1, 2),
#     regimen_order_type = "within cancer"
#   )
#
#   test_4b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(grepl("Carboplatin, Pemetrexed Disodium", regimen_drugs)) %>%
#     filter(new_reg_number %in% c(1, 2)) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_4a$cohort_ca_drugs, test_4b)
# })
#
#
# test_that("drug regimen specified, order specified to be within regimen", {
#   # single regimen specified, want first time that regimen was given for all
#   # cancers
#   test_1a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
#     regimen_order = c(1),
#     regimen_order_type = "within REGimen"
#   )
#
#   test_1b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id, regimen_drugs) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium") %>%
#     filter(new_reg_number %in% c(1)) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_1a$cohort_ca_drugs, test_1b)
#
#   # multiple regimens specified, want first time each given
#   test_2a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
#     regimen_order = c(1),
#     regimen_order_type = "within REGimen"
#   )
#
#   test_2b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id, regimen_drugs) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(regimen_drugs %in% c(
#       "Carboplatin, Pemetrexed Disodium",
#       "Nivolumab"
#     )) %>%
#     filter(new_reg_number %in% c(1)) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_2a$cohort_ca_drugs, test_2b)
#
#   # multiple regimens specified
#   # first and/or second time they were received
#   # multiple regimens specified, want first time each given
#   test_3a <- create_analytic_cohort(
#     cohort = "NSCLC",
#     data_synapse = nsclc_data,
#     regimen_drugs = c("Carboplatin, Pemetrexed Disodium", "Nivolumab"),
#     regimen_order = c(1, 2),
#     regimen_order_type = "within REGimen"
#   )
#
#   test_3b <- left_join(nsclc_data$ca_dx_index_NSCLC %>%
#     group_by(record_id) %>%
#     slice(which.min(ca_seq)),
#   nsclc_data$ca_drugs_NSCLC,
#   by = c(
#     "cohort", "record_id", "ca_seq",
#     "institution"
#   )
#   ) %>%
#     group_by(record_id, regimen_drugs) %>%
#     mutate(new_reg_number = 1:n()) %>%
#     ungroup() %>%
#     filter(regimen_drugs %in% c(
#       "Carboplatin, Pemetrexed Disodium",
#       "Nivolumab"
#     )) %>%
#     filter(new_reg_number %in% c(1, 2)) %>%
#     select(-new_reg_number)
#
#   expect_equal(test_3a$cohort_ca_drugs, test_3b)
# })
