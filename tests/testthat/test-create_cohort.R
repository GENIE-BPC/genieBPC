# adding this one fake test to initialize the unit testing framework

# test that three datasets are returned from create_cohort
test_that("correct number of objects returned from create cohort", {
  nsclc_data <- pull_data_synapse(c("NSCLC"), version = "1.1")

  test1 <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    return_summary = FALSE
  )

  expect_equal(length(test1), 3)

  test2 <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    return_summary = TRUE
  )

  expect_equal(length(test2), 5)
})

test_that("correct cohort returned from create cohort", {
  nsclc_data <- pull_data_synapse(c("NSCLC"), version = "1.1")

  test1 <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    return_summary = FALSE
  )

  expect_equal(unique(test1$cohort_ca_dx$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_ca_drugs$cohort), "NSCLC")
  expect_equal(unique(test1$cohort_cpt$cohort), "NSCLC")

  # check CRC
  crc_data <- pull_data_synapse(c("CRC"), version = "1.1")

  test2 <- create_cohort(
    cohort = "CRC",
    cohort_object = crc_data,
    return_summary = FALSE
  )

  expect_equal(unique(test2$cohort_ca_dx$cohort), "CRC")
  expect_equal(unique(test2$cohort_ca_drugs$cohort), "CRC")
  expect_equal(unique(test2$cohort_cpt$cohort), "CRC")

  # non-existent cohort is specified
  expect_error(create_cohort(cohort = "Lung cancer",
                             cohort_object = nsclc_data))
})

test_that("correct diagnosis/diagnoses returned", {
  nsclc_data <- pull_data_synapse("NSCLC", version = "1.1")

  # scenario 1: no diagnosis criteria specified
  # expect that the first index cancer is returned
  test1a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data
  )

  test1b <- nsclc_data$ca_dx_index_NSCLC %>%
    group_by(record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup()

  expect_equal(test1a$cohort_ca_dx, test1b)

  # scenario 2: first OR second index cancer is specified
  # if patient only has 1 index cancer, it should be returned
  # if patient has 2+ index cancers, the first two should be returned
  test2a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    index_ca_seq = c(1, 2),
    return_summary = TRUE
  )

  test2b <- nsclc_data$ca_dx_index_NSCLC %>%
    group_by(cohort, record_id) %>%
    arrange(cohort, record_id, ca_seq) %>%
    mutate(index_ca_seq = 1:n()) %>%
    ungroup() %>%
    filter(index_ca_seq %in% c(1, 2)) %>%
    select(-index_ca_seq)

  expect_equal(test2a$cohort_ca_dx, test2b)

  # scenario 3: an index cancer # that doesn't exist in the data is specified
  expect_error(create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    index_ca_seq = 10))

})

  # scenario 3: histology is specified
  test3a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    ca_hist_adeno_squamous = "Adenocarcinoma"
  )

  test3b <- nsclc_data$ca_dx_index_NSCLC %>%
    group_by(cohort, record_id) %>%
    slice(which.min(ca_seq)) %>%
    ungroup() %>%
    filter(ca_hist_adeno_squamous == "Adenocarcinoma")

  expect_equal(test3a$cohort_ca_dx, test3b)

  test_that("correct regimen returned", {
  # scenario 3: want all index cancers returned
  # if patient only has 1 index cancer, it should be returned
  # if patient has multiple, all should be returned
  test3a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_data,
    index_ca_seq = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    return_summary = TRUE
  )

  test3b <- nsclc_data$ca_dx_index_NSCLC
  expect_equal(test3a$cohort_ca_dx, test3b)
})

test_that("correct regimen returned", {
  nsclc_crc_data <- pull_data_synapse(c("NSCLC"))

  # scenario 1: no drug regimen is specified, all drug regimens are returned
  # for a patient's 1st index cancer
  test1a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_crc_data
  )

  expect_equal(test1a$cohort_ca_drugs, nsclc_crc_data$ca_drugs_NSCLC)
  nrow(test1a$cohort_ca_drugs)
  nrow(nsclc_crc_data$ca_drugs_NSCLC)
  })

  # scenario 1: all patients whose first drug after diagnosis was carbo pem
  test1a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_crc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = 1,
    regimen_order_type = "within cancer",
    regimen_type = "Exact",
    return_summary = TRUE
  )

  # compare to data
  test1b <- left_join(nsclc_crc_data$ca_dx_index_NSCLC %>%
              group_by(record_id) %>%
              slice(which.min(ca_seq)),
            nsclc_crc_data$ca_drugs_NSCLC,
            by = c("cohort", "record_id", "ca_seq", "institution")) %>%
    group_by(record_id) %>%
    slice(which.min(regimen_number)) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium")

  expect_equal(test1a$cohort_ca_drugs, test1b)

  # test 2: all patients whose *second* drug after diagnosis was carbo pem
  test2a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_crc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = 2,
    regimen_order_type = "within cancer",
    regimen_type = "Exact",
    return_summary = TRUE
  )

  test2b <- left_join(nsclc_crc_data$ca_dx_index_NSCLC %>%
                        group_by(record_id) %>%
                        slice(which.min(ca_seq)),
                      nsclc_crc_data$ca_drugs_NSCLC,
                      by = c("cohort", "record_id", "ca_seq", "institution")) %>%
    group_by(record_id) %>%
    mutate(reg_num_new = 1:n()) %>%
    ungroup() %>%
    filter(reg_num_new == 2) %>%
    select(-reg_num_new) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium")

  expect_equal(test2a$cohort_ca_drugs, test2b)

  # test 3: all patients whose *second* drug after diagnosis was carbo pem
  test3a <- create_cohort(
    cohort = "NSCLC",
    cohort_object = nsclc_crc_data,
    regimen_drugs = c("Carboplatin, Pemetrexed Disodium"),
    regimen_order = 2,
    regimen_order_type = "within cancer",
    regimen_type = "Exact",
    return_summary = TRUE
  )

  test3b <- left_join(nsclc_crc_data$ca_dx_index_NSCLC %>%
                        group_by(record_id) %>%
                        slice(which.min(ca_seq)),
                      nsclc_crc_data$ca_drugs_NSCLC,
                      by = c("cohort", "record_id", "ca_seq", "institution")) %>%
    group_by(record_id) %>%
    mutate(reg_num_new = 1:n()) %>%
    ungroup() %>%
    filter(reg_num_new == 2) %>%
    select(-reg_num_new) %>%
    ungroup() %>%
    filter(regimen_drugs == "Carboplatin, Pemetrexed Disodium")

  expect_equal(test2a$cohort_ca_drugs, test2b)
})
