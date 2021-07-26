

test_that("Test class and length of list for NSCLC",{
nsclc_data <- pull_data_synapse(c("NSCLC"), version = "1.1")
expect_equal(length(nsclc_data ),11)
expect_equal(nsclc_data,"list")
})


test_that("Test class length of list for CRC",{
  crc_data <- pull_data_synapse(c("CRC"), version = "1.1")
  expect_equal(length(crc_data) , 12)
  expect_equal(class(crc_data),"list")
})


test_that("Number of columns and rows for each NSCLC dataset",{
  nsclc_data <- pull_data_synapse(c("NSCLC"), version = "1.1")
  col_length <- sapply(nsclc_data, length)
  row_length <- sapply(nsclc_data, nrow)
  names(col_length) <- NULL
  names(row_length) <- NULL
  expect_equal(col_length, c(33,110,83,114,195,42,11,19,1782,9,54))
  expect_equal(row_length,c(1849,1874,810,4032,8329,35113,24950,2026,930,821,17574))
wow})



test_that("Number of columns and rows for each CRC dataset",{
  crc_data <- pull_data_synapse(c("CRC"), version = "1.1")
  col_length <- sapply(crc_data, length)
  row_length <- sapply(crc_data, nrow)
  names(col_length) <- NULL
  names(row_length) <- NULL
  expect_equal(col_length, c(37,111,87,102,340,42,11,12,25,1505,9,54))
  expect_equal(row_length, c(1500,1510,353,5459,7216,26500,28467,24708,1576,930,406,23445))
  })

test_that("Testing synapse version",{
  expect_equal(class(synapse_version(FALSE)),c("grouped_df", "tbl_df","tbl", "data.frame"))
  expect_equal(class(synapse_version(TRUE)),c("grouped_df", "tbl_df","tbl", "data.frame"))

  expect_equal(synapse_version(TRUE) |>
      dplyr::count(cohort) |>
      dplyr::ungroup() |>
      dplyr::distinct(n) |>
        as.data.frame(),data.frame(n=1))

})
