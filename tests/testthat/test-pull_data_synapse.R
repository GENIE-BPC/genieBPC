# Consortium: Pull Data With PAT -------------------------------

test_that("Test class and length of list for public data", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # compare to expected length
  expect_equal(data_releases$expected_n_dfs, actual_length$length)

  # compare to expected class
  # expect each data release returned to be a list, need to rep "list" the
  # number of times for the data releases we have
  expect_equal(unname(map_chr(test_list, class)), rep("list", nrow(data_releases)))
})

# * Check Arguments ----------------------------------------------

test_that("Missing cohort parameter", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))
  expect_error(pull_data_synapse())

})


test_that("test `cohort` argument specification", {

  # try to misspecify cohort (lower cases instead of capital)
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))
  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))

  # expect lower case cohort to work
  expect_equal(pull_data_synapse(
    cohort = "NSCLC",
    version = "v2.2-consortium"
  ),
  pull_data_synapse(
    cohort = "nsclc",
    version = "v2.2-consortium"
  ))

  expect_equal(pull_data_synapse(
    cohort = "CRC",
    version = "v2.0-public"
  ),
  pull_data_synapse(
    cohort = "crc",
    version = "v2.0-public"
  ))

  expect_equal(pull_data_synapse(
    cohort = "BrCa",
    version = "v1.2-consortium"
  ),
  pull_data_synapse(
    cohort = "brca",
    version = "v1.2-consortium"
  ))

  expect_equal(pull_data_synapse(
    cohort = "PANC",
    version = "v1.2-consortium"
  ),
  pull_data_synapse(
    cohort = "pancreas",
    version = "v1.2-consortium"
  ))

  expect_equal(pull_data_synapse(
    cohort = "Prostate",
    version = "v1.2-consortium"
  ),
  pull_data_synapse(
    cohort = "PrOsTaTe",
    version = "v1.2-consortium"
  ))

  expect_equal(pull_data_synapse(
    cohort = "BLADDER",
    version = "v1.2-consortium"
  ),
  pull_data_synapse(
    cohort = "bladdER",
    version = "v1.2-consortium"
  ))

  # try to misspecify cohort
  expect_error(pull_data_synapse(
    cohort = "nsclc3",
    version = "v2.2-consortium"
  ), "`cohort` must be one of*")
})

test_that("test `version` argument specification", {
  testthat::skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

  # no version specified
  expect_error(
    pull_data_synapse(
      cohort = "NSCLC",
      version = NULL
    ),
    "Version needs to be specified.*"
  )

  # incorrect arg formatting
  expect_error(pull_data_synapse(
    cohort = "NSCLC",
    version = "1.1"
  ), "*")

  # more versions than cohorts
  expect_error(
    pull_data_synapse(
      cohort = "NSCLC",
      version = c(
        "v2.2-consortium",
        "v2.0-public"
      )
    ),
    "*You have selected"
  )


  # mismatch version-cohort
  expect_error(
    pull_data_synapse(
      cohort = "BrCa",
      version = c("v2.1-consortium")
    ),
    "You have selected a version that is not available for this cohort*"
  )
})

# * Check Results of Consortium Pull ----------------------------------------------

test_that("correct release returned", {
  # exit if user doesn't have a synapse log in or access to data.
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # not all data releases had a release_version variable
  test_list_release_version_avail <- within(test_list,
                                            rm(list = ls(pattern = "NSCLC_v1.1-consortium")))

  # for each data frame returned with a cohort, get the release_version variable
  # remove genomic data frames since we don't expect them to have a release_version variable
  test_list_release_version_avail_no_genomic <- map_depth(test_list_release_version_avail,
                                                          .depth = 2,
                                                ~purrr::discard_at(.x, c("cna",
                                                                  "fusions",
                                                                  "sv",
                                                                  "mutations_extended")))

  # for each dataframe returned for a data release, get the cohort variable
  release_returned <- map_depth(test_list_release_version_avail_no_genomic, .depth = 3, select,
                                any_of("release_version")) %>%
    map_depth(., .depth = 2, enframe) %>%
    map_depth(., .depth = 2, unnest, cols = value) %>%
    map_depth(., .depth = 2, distinct) %>%
    map(., 1) %>%
    # sometimes release_version is char and sometimes numeric, make char
    map(., mutate, release_version_character = str_replace(pattern = "pharma",
                                                           replacement = "consortium",
                                                           string = as.character(release_version))) %>%
    map(., select, -release_version) %>%
    bind_rows(.id = "data_release") %>%
    # PANC v1.2-consortium and Prosate v1.2-consortium are missing
    # release_version_character (it isn't on the data for some reason)
    # need to drop missing values to avoid error in tests
    drop_na(release_version_character) %>%
    mutate(release_matches = str_detect(pattern = str_trim(release_version_character),
                                        string  = data_release)) %>%
    filter(release_matches == FALSE)

  expect_equal(nrow(release_returned), 0)
})

test_that("Number of columns and rows for each data release", {
  testthat::skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

  # get number of columns for each dataframe returned
  col_lengths <- map_depth(test_list, .depth = 3, length) %>%
    map_depth(., .depth = 2, bind_rows) %>%
    map(., 1, .id = "test") %>%
    map(., pivot_longer, cols = everything(), names_to = "df", values_to = "ncol") # %>%
  # map_df(., bind_rows, .id = "data_release")

  # get nrow for each dataframe returned
  row_lengths <- map_depth(test_list, .depth = 3, nrow) %>%
    map_depth(., .depth = 2, bind_rows) %>%
    map(., 1, .id = "test") %>%
    map(., pivot_longer, cols = everything(), names_to = "df", values_to = "nrow")
  # map_df(., bind_rows, .id = "data_release")

  # internal df of expected number of rows and columns
  expected_length <- genieBPC:::data_releases_expected_size %>%
    filter(data_release %in% names(test_list)) %>%
    mutate(data_release_factor = factor(data_release,
                                        levels = paste0(data_releases$cohort,
                                                        "_",
                                                        data_releases$version))) %>%
    # separate(data_release, into = c("cohort", "data_release"), sep = "_") %>%
    split(~data_release_factor)

  # expect the correct number of columns
  map2(
    map(col_lengths, pull, "ncol"),
    map(expected_length, pull, expected_ncol),
    expect_equal
  )

  # expect the correct number of rows
  map2(
    map(row_lengths, pull, "nrow"),
    map(expected_length, pull, expected_nrow),
    expect_equal
  )
})

test_that("Test NA conversion", {
  testthat::skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

  # making sure there are no character "" instead of NAs
  # count number of character "" across all columns
  # first count number of times each column has a ""
  any_blank_cols <- map_depth(test_list, .depth = 3,
                              ~colSums(.x == "", na.rm = TRUE)) %>%
    # then aggregate across dataframes
    map_depth(., .depth = 3, ~sum(.x)) %>%
    # set all dataframes together
    map_depth(., .depth = 2, bind_rows) %>%
    map_depth(., .depth = 1, 1) %>%
    bind_rows(., .id = "release") %>%
    # get 1 row/release/df
    pivot_longer(cols = c(everything(), -release),
                 names_to = "df",
                 values_to = "n_blanks") %>%
    filter(n_blanks != 0)

  expect_equal(nrow(any_blank_cols), 0)
})

# # Public: Pull Public Data With PAT -------------------------------

test_that("Test class and length of list for public data", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))

  data_releases_pat <- synapse_tables %>%
    distinct(cohort, version) %>%
    filter(grepl("public", version))

  test_list <- expect_no_error(pmap(select(data_releases_pat, cohort, version),
                                    pull_data_synapse))

})

# # Consortium: Try Consortium Data With PUBLIC PAT -------------------------------


test_that("Test that trying to pull consortium with public data fails", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))

  data_releases_consortium <- synapse_tables %>%
    filter(str_detect(version, "consortium")) %>%
    distinct(cohort, version) %>%
    head(n = 1)

  expect_error(
    pull_data_synapse(cohort = data_releases_consortium$cohort,
                      version = data_releases_consortium$version))

})

# # Public: Pull Public Data With PAT --------------------------------------------------------

testthat::expect_true(
  if (.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))) {

    set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))

    # Get Public Releases Only
    # since so few public data releases to date and because they are publicly available,
    # leave this to test all public data releases
    data_releases_public <- synapse_tables %>%
      distinct(cohort, version) %>%
      filter(str_detect(version, "public")) %>%
      # merge on expected size
      inner_join(count(genieBPC:::data_releases_expected_size, cohort, version,
                       name = "expected_n_dfs"),
                 by = c("cohort", "version"))

    # for each data release, pull data into the R environment
    test_list <- pmap(data_releases_public %>%
                        select(cohort, version),
                      pull_data_synapse)

    # name the items in the list
    names(test_list) <- paste0(
      data_releases_public$cohort, "_",
      data_releases_public$version
    )

    # get actual length of each data release returned from pull_data_synapse
    actual_length <- map_depth(test_list, .depth = 2, length) %>%
      bind_rows() %>%
      pivot_longer(
        cols = everything(),
        names_to = "data_release",
        values_to = "length",
        values_drop_na = TRUE
      )

    length(actual_length) > 0
  } else {0 == 0})



test_that("Test class and length of list for public data", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  # compare to expected length
  expect_equal(data_releases_public$expected_n_dfs, actual_length$length)

  # compare to expected class
  # expect each data release returned to be a list, need to rep "list" the
  # number of times for the data releases we have
  expect_equal(unname(map_chr(test_list, class)), rep("list", nrow(data_releases_public)))
})



# Public: Pull Public Data With PAT -----------------------------
test_that("Test no error with Public access PAT, not consortium specific check", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))

  expect_no_error(pull_data_synapse(
    cohort = "NSCLC",
    version = "v2.0-public",
    pat =  Sys.getenv("SYNAPSE_PAT_PUBLIC")
  ))

})


# No Terms: Pull Public Data With PAT -----------------------------

test_that("Test error when access terms not checked", {
  testthat::skip_if_not(.is_connected_to_genie(
    pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")
  ))

  set_synapse_credentials(
    pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")
  )

  expect_error(pull_data_synapse(
    cohort = "CRC",
    version = "v2.0-public"
  ))

})

# No Terms: Pull Public Data With PAT -----------------------------------------

# Can terms error message be improved? It's layered and confusing rn but can't think
# of best way to fix it
# <Maybe Needs more tests>

test_that("Test error when access terms not checked", {
  testthat::skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")))
  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS"))

    expect_error(
      pull_data_synapse(
        cohort = "CRC",
        version = "v2.0-public"
      )
    )

})

