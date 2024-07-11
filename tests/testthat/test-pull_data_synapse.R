# Consortium: Pull Consortium Data With PAT -------------------------------

# return to avoid having to re-run pull_data_synapse for
# each test
testthat::expect_true(
  if(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))) {

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))

  # data frame of each release to use for pmap
  data_releases <- synapse_tables %>%
    distinct(cohort, version) %>%
    # define expected number of dataframes based on whether TM and RT data were released
    mutate(expected_n_dfs = case_when(
      ## no TM or RT
      cohort == "NSCLC" & version %in% c("v1.1-consortium", "v2.0-public") ~ 11,
      # sv file added to NSCLC at 2.2-consortium
      cohort == "NSCLC" ~ 12,
      ## TM, no RT
      cohort %in% c("CRC") & version == "v2.0-public" ~ 12,
      cohort %in% c("BrCa") ~ 12,
      # sv added
      cohort %in% c("CRC") ~ 13,
      ## RT, no TM
      cohort == "BLADDER" & version == "v1.1-consortium" ~ 12,
      # sv added
      cohort == "BLADDER" & version == "v1.2-consortium" ~ 13,
      # TM and RT
      cohort %in% c("PANC", "Prostate") ~ 13
    ))

  # for each data release, pull data into the R environment
  test_list <- pmap(data_releases %>%
                      select(cohort, version),
                    pull_data_synapse)

  # name the items in the list
  names(test_list) <- paste0(
    data_releases$cohort, "_",
    data_releases$version
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
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

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
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))
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
  skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

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
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT")))

  # not all data releases had a release_version variable
  test_list_release_version_avail <- within(test_list,
                                            rm(`NSCLC_v1.1-consortium`))

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
    mutate(release_matches = str_detect(pattern = str_trim(release_version_character),
                                        string = data_release)) %>%
    filter(release_matches == FALSE)

  expect_equal(nrow(release_returned), 0)
})

test_that("Number of columns and rows for each data release", {
  skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

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

  # hard coded table of expected number of rows and columns
  # requires update for each data release
  expected_length <- tibble::tribble(
    ~data_release, ~df, ~expected_nrow, ~expected_ncol,
    ## NSCLC
    # v1.1-consortium
    "NSCLC_v1.1-consortium", "pt_char", 1849, 33,
    "NSCLC_v1.1-consortium", "ca_dx_index", 1874, 110,
    "NSCLC_v1.1-consortium", "ca_dx_non_index", 810, 83,
    "NSCLC_v1.1-consortium", "ca_drugs", 4032, 114,
    "NSCLC_v1.1-consortium", "prissmm_imaging", 35113, 42,
    "NSCLC_v1.1-consortium", "prissmm_pathology", 8329, 195,
    "NSCLC_v1.1-consortium", "prissmm_md", 24950, 11,
    "NSCLC_v1.1-consortium", "cpt", 2026, 19,
    "NSCLC_v1.1-consortium", "mutations_extended", 17574, 54,
    "NSCLC_v1.1-consortium", "fusions", 821, 9,
    "NSCLC_v1.1-consortium", "cna", 930, 1782,
    # NSCLC 2.2-consortium
    "NSCLC_v2.2-consortium", "pt_char", 1832, 35,
    "NSCLC_v2.2-consortium", "ca_dx_index", 1858, 152,
    "NSCLC_v2.2-consortium", "ca_dx_non_index", 791, 97,
    "NSCLC_v2.2-consortium", "ca_drugs", 4012, 101,
    "NSCLC_v2.2-consortium", "prissmm_imaging", 34926, 43,
    "NSCLC_v2.2-consortium", "prissmm_pathology", 8277, 196,
    "NSCLC_v2.2-consortium", "prissmm_md", 24804, 12,
    "NSCLC_v2.2-consortium", "cpt", 2002, 26,
    "NSCLC_v2.2-consortium", "mutations_extended", 17430, 64,
    "NSCLC_v2.2-consortium", "fusions", 815, 9,
    "NSCLC_v2.2-consortium", "sv", 638, 12,
    "NSCLC_v2.2-consortium", "cna", 965, 1764,
    # v2.0-public
    "NSCLC_v2.0-public", "pt_char", 1846, 36,
    "NSCLC_v2.0-public", "ca_dx_index", 1869, 141,
    "NSCLC_v2.0-public", "ca_dx_non_index", 797, 86,
    "NSCLC_v2.0-public", "ca_drugs", 4032, 102,
    "NSCLC_v2.0-public", "prissmm_imaging", 35101, 42,
    "NSCLC_v2.0-public", "prissmm_pathology", 8342, 196,
    "NSCLC_v2.0-public", "prissmm_md", 24909, 12,
    "NSCLC_v2.0-public", "cpt", 2015, 28,
    "NSCLC_v2.0-public", "mutations_extended", 17472, 64,
    "NSCLC_v2.0-public", "fusions", 819, 9,
    "NSCLC_v2.0-public", "cna", 964, 1779,
    # v3.1-consortium
    "NSCLC_v3.1-consortium", "pt_char", 3549, 38,
    "NSCLC_v3.1-consortium", "ca_dx_index", 3671, 143,
    "NSCLC_v3.1-consortium", "ca_dx_non_index", 1719, 106,
    "NSCLC_v3.1-consortium", "ca_drugs", 7930, 103,
    "NSCLC_v3.1-consortium", "prissmm_imaging", 72735, 43,
    "NSCLC_v3.1-consortium", "prissmm_pathology", 16665, 196,
    "NSCLC_v3.1-consortium", "ca_radtx", 5038, 84,
    "NSCLC_v3.1-consortium", "prissmm_md", 50578, 15,
    "NSCLC_v3.1-consortium", "cpt", 4238, 29,
    "NSCLC_v3.1-consortium", "mutations_extended", 42426, 64,
    "NSCLC_v3.1-consortium", "sv", 1641, 40,
    "NSCLC_v3.1-consortium", "cna", 1000, 3702,
    ## CRC
    # v1.3-consortium
    "CRC_v1.3-consortium", "pt_char", 1476, 40,
    "CRC_v1.3-consortium", "ca_dx_index", 1485, 152,
    "CRC_v1.3-consortium", "ca_dx_non_index", 328, 97,
    "CRC_v1.3-consortium", "ca_drugs", 5401, 102,
    "CRC_v1.3-consortium", "prissmm_imaging", 26091, 43,
    "CRC_v1.3-consortium", "prissmm_pathology", 7112, 341,
    "CRC_v1.3-consortium", "prissmm_md", 27954, 12,
    "CRC_v1.3-consortium", "tumor_marker", 24219, 14,
    "CRC_v1.3-consortium", "cpt", 1551, 26,
    "CRC_v1.3-consortium", "mutations_extended", 22903, 64,
    "CRC_v1.3-consortium", "fusions", 395, 9,
    "CRC_v1.3-consortium", "sv", 324, 12,
    "CRC_v1.3-consortium", "cna", 965, 1479,
    # 2.0-public
    "CRC_v2.0-public", "pt_char", 1485, 51,
    "CRC_v2.0-public", "ca_dx_index", 1494, 142,
    "CRC_v2.0-public", "ca_dx_non_index", 336, 106,
    "CRC_v2.0-public", "ca_drugs", 5417, 103,
    "CRC_v2.0-public", "prissmm_imaging", 26260, 42,
    "CRC_v2.0-public", "prissmm_pathology", 7156, 320,
    "CRC_v2.0-public", "prissmm_md", 28164, 12,
    "CRC_v2.0-public", "tumor_marker", 24462, 14,
    "CRC_v2.0-public", "cpt", 1559, 29,
    "CRC_v2.0-public", "mutations_extended", 23225, 64,
    "CRC_v2.0-public", "fusions", 403, 9,
    "CRC_v2.0-public", "cna", 965, 1488,
    ## BrCa
    "BrCa_v1.1-consortium", "pt_char", 1130, 40,
    "BrCa_v1.1-consortium", "ca_dx_index", 1141, 159,
    "BrCa_v1.1-consortium", "ca_dx_non_index", 194, 103,
    "BrCa_v1.1-consortium", "ca_drugs", 6906, 102,
    "BrCa_v1.1-consortium", "prissmm_imaging", 26763, 43,
    "BrCa_v1.1-consortium", "prissmm_pathology", 7223, 371,
    "BrCa_v1.1-consortium", "prissmm_md", 28293, 12,
    "BrCa_v1.1-consortium", "tumor_marker", 9744, 14,
    "BrCa_v1.1-consortium", "cpt", 1234, 27,
    "BrCa_v1.1-consortium", "mutations_extended", 6633, 54,
    "BrCa_v1.1-consortium", "fusions", 610, 9,
    "BrCa_v1.1-consortium", "cna", 930, 1222,
    # v1.2-consortium
    "BrCa_v1.2-consortium", "pt_char", 1129, 40,
    "BrCa_v1.2-consortium", "ca_dx_index", 1140, 159,
    "BrCa_v1.2-consortium", "ca_dx_non_index", 194, 94,
    "BrCa_v1.2-consortium", "ca_drugs", 6896, 110,
    "BrCa_v1.2-consortium", "prissmm_imaging", 26747, 43,
    "BrCa_v1.2-consortium", "prissmm_pathology", 7210, 356,
    "BrCa_v1.2-consortium", "prissmm_md", 28264, 12,
    "BrCa_v1.2-consortium", "tumor_marker", 9744, 14,
    "BrCa_v1.2-consortium", "cpt", 1233, 27,
    "BrCa_v1.2-consortium", "mutations_extended", 6646, 64,
    "BrCa_v1.2-consortium", "fusions", 611, 9,
    "BrCa_v1.2-consortium", "cna", 965, 1222,
    ## PANC
    # v1.1-consortium
    "PANC_v1.1-consortium", "pt_char", 1109, 52,
    "PANC_v1.1-consortium", "ca_dx_index", 1110, 141,
    "PANC_v1.1-consortium", "ca_dx_non_index", 279, 108,
    "PANC_v1.1-consortium", "ca_drugs", 3153, 102,
    "PANC_v1.1-consortium", "prissmm_imaging", 14521, 42,
    "PANC_v1.1-consortium", "prissmm_pathology", 3532, 289,
    "PANC_v1.1-consortium", "ca_radtx", 527, 83,
    "PANC_v1.1-consortium", "prissmm_md", 15870, 13,
    "PANC_v1.1-consortium", "tumor_marker", 17720, 15,
    "PANC_v1.1-consortium", "cpt", 1130, 29,
    "PANC_v1.1-consortium", "mutations_extended", 6572, 64,
    "PANC_v1.1-consortium", "fusions", 330, 9,
    "PANC_v1.1-consortium", "cna", 965, 1059,
    # v1.2-consortium
    "PANC_v1.2-consortium", "pt_char", 1109, 52,
    "PANC_v1.2-consortium", "ca_dx_index", 1110, 141,
    "PANC_v1.2-consortium", "ca_dx_non_index", 279, 108,
    "PANC_v1.2-consortium", "ca_drugs", 3153, 102,
    "PANC_v1.2-consortium", "prissmm_imaging", 14521, 42,
    "PANC_v1.2-consortium", "prissmm_pathology", 3532, 289,
    "PANC_v1.2-consortium", "ca_radtx", 527, 82,
    "PANC_v1.2-consortium", "prissmm_md", 15870, 13,
    "PANC_v1.2-consortium", "tumor_marker", 17720, 15,
    "PANC_v1.2-consortium", "cpt", 1130, 29,
    "PANC_v1.2-consortium", "mutations_extended", 6567, 64,
    "PANC_v1.2-consortium", "fusions", 330, 9,
    "PANC_v1.2-consortium", "cna", 965, 1058,
    ## Prostate
    # v1.1-consortium
    "Prostate_v1.1-consortium", "pt_char", 1116, 53,
    "Prostate_v1.1-consortium", "ca_dx_index", 1116, 145,
    "Prostate_v1.1-consortium", "ca_dx_non_index", 180, 107,
    "Prostate_v1.1-consortium", "ca_drugs", 5588, 102,
    "Prostate_v1.1-consortium", "prissmm_imaging", 22496, 42,
    "Prostate_v1.1-consortium", "prissmm_pathology", 5121, 234,
    "Prostate_v1.1-consortium", "ca_radtx", 1953, 81,
    "Prostate_v1.1-consortium", "prissmm_md", 21733, 15,
    "Prostate_v1.1-consortium", "tumor_marker", 41411, 15,
    "Prostate_v1.1-consortium", "cpt", 1227, 29,
    "Prostate_v1.1-consortium", "mutations_extended", 6194, 64,
    "Prostate_v1.1-consortium", "fusions", 1148, 9,
    "Prostate_v1.1-consortium", "cna", 965, 1168,
    # v1.2-consortium
    "Prostate_v1.2-consortium", "pt_char", 1116, 53,
    "Prostate_v1.2-consortium", "ca_dx_index", 1116, 145,
    "Prostate_v1.2-consortium", "ca_dx_non_index", 180, 107,
    "Prostate_v1.2-consortium", "ca_drugs", 5588, 102,
    "Prostate_v1.2-consortium", "prissmm_imaging", 22496, 42,
    "Prostate_v1.2-consortium", "prissmm_pathology", 5121, 234,
    "Prostate_v1.2-consortium", "ca_radtx", 1953, 80,
    "Prostate_v1.2-consortium", "prissmm_md", 21733, 15,
    "Prostate_v1.2-consortium", "tumor_marker", 41411, 15,
    "Prostate_v1.2-consortium", "cpt", 1227, 29,
    "Prostate_v1.2-consortium", "mutations_extended", 6193, 64,
    "Prostate_v1.2-consortium", "fusions", 1148, 9,
    "Prostate_v1.2-consortium", "cna", 965, 1167,
    ## Bladder
    # v1.1-consortium
    "BLADDER_v1.1-consortium", "pt_char", 716, 39,
    "BLADDER_v1.1-consortium", "ca_dx_index", 716, 143,
    "BLADDER_v1.1-consortium", "ca_dx_non_index", 523, 111,
    "BLADDER_v1.1-consortium", "ca_drugs", 2269, 103,
    "BLADDER_v1.1-consortium", "prissmm_imaging", 13563, 42,
    "BLADDER_v1.1-consortium", "prissmm_pathology", 7944, 389,
    "BLADDER_v1.1-consortium", "ca_radtx", 533, 81,
    "BLADDER_v1.1-consortium", "prissmm_md", 10367, 15,
    "BLADDER_v1.1-consortium", "cpt", 748, 29,
    "BLADDER_v1.1-consortium", "mutations_extended", 11000, 64,
    "BLADDER_v1.1-consortium", "fusions", 242, 9,
    "BLADDER_v1.1-consortium", "cna", 965, 698,
    # v1.2-consortium
    "BLADDER_v1.2-consortium", "pt_char", 716, 39,
    "BLADDER_v1.2-consortium", "ca_dx_index", 716, 143,
    "BLADDER_v1.2-consortium", "ca_dx_non_index", 523, 111,
    "BLADDER_v1.2-consortium", "ca_drugs", 2269, 103,
    "BLADDER_v1.2-consortium", "prissmm_imaging", 13563, 42,
    "BLADDER_v1.2-consortium", "prissmm_pathology", 7944, 374,
    "BLADDER_v1.2-consortium", "ca_radtx", 533, 81,
    "BLADDER_v1.2-consortium", "prissmm_md", 10367, 15,
    "BLADDER_v1.2-consortium", "cpt", 748, 29,
    "BLADDER_v1.2-consortium", "mutations_extended", 12994, 64,
    "BLADDER_v1.2-consortium", "fusions", 242, 9,
    "BLADDER_v1.2-consortium", "sv", 202, 40,
    "BLADDER_v1.2-consortium", "cna", 999, 674
  ) %>%
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
  skip_if_not(.is_connected_to_genie(set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT"))))

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

# # Consortium: Pull Consortium Data With Username/Password -------------------------------

test_that("Test class and length of list for public data", {
  skip_if_not(.is_connected_to_genie(username = Sys.getenv("SYNAPSE_USERNAME"),
                                     password = Sys.getenv("SYNAPSE_PASSWORD")))

  set_synapse_credentials(username = Sys.getenv("SYNAPSE_USERNAME"),
                          password = Sys.getenv("SYNAPSE_PASSWORD"),
                          pat = NULL)

  data_releases_username <- synapse_tables %>%
    distinct(cohort, version) %>%
    head(n = 2)

  test_list <- expect_no_error(pmap(select(data_releases_username, cohort, version),
                                    pull_data_synapse))

})

# # Consortium: Try Consortium Data With PUBLIC Username/Password -------------------------------


test_that("Test that trying to pull consortium with public data fails", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

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
    data_releases_public <- synapse_tables %>%
      distinct(cohort, version) %>%
      filter(str_detect(version, "public")) %>%
      # define expected number of dataframes based on whether TM and RT data were released
      mutate(expected_n_dfs = case_when(
        ## no TM or RT
        cohort == "NSCLC" & version %in% c("v1.1-consortium", "v2.0-public") ~ 11,
        # sv file added to NSCLC at 2.2-consortium
        cohort == "NSCLC" ~ 12,
        ## TM, no RT
        cohort %in% c("CRC") & version == "v2.0-public" ~ 12,
        cohort %in% c("BrCa") ~ 12,
        # sv added
        cohort %in% c("CRC") ~ 13,
        ## RT, no TM
        cohort == "BLADDER" & version == "v1.1-consortium" ~ 12,
        # sv added
        cohort == "BLADDER" & version == "v1.2-consortium" ~ 13,
        # TM and RT
        cohort %in% c("PANC", "Prostate") ~ 13
      ))

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
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  # compare to expected length
  expect_equal(data_releases_public$expected_n_dfs, actual_length$length)

  # compare to expected class
  # expect each data release returned to be a list, need to rep "list" the
  # number of times for the data releases we have
  expect_equal(unname(map_chr(test_list, class)), rep("list", nrow(data_releases_public)))
})



# Public: Pull Public Data With Username/Password -----------------------------
test_that("Test no error with Public access PAT, not consortium specific check", {
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC")))

  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_PUBLIC"))

  expect_no_error(pull_data_synapse(
    cohort = "NSCLC",
    version = "v2.0-public",
    pat =  Sys.getenv("SYNAPSE_PAT_PUBLIC")
  ))

})


# No Terms: Pull Public Data With Username/Password -----------------------------

test_that("Test error when access terms not checked", {
  skip_if_not(.is_connected_to_genie(
    username = Sys.getenv("SYNAPSE_USERNAME_NO_TERMS"),
    password = Sys.getenv("SYNAPSE_PASSWORD_NO_TERMS")
  ))

  set_synapse_credentials(
    username = Sys.getenv("SYNAPSE_USERNAME_NO_TERMS"),
    password = Sys.getenv("SYNAPSE_PASSWORD_NO_TERMS")
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
  skip_if_not(.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS")))
  set_synapse_credentials(pat = Sys.getenv("SYNAPSE_PAT_NO_TERMS"))

    expect_error(
      pull_data_synapse(
        cohort = "CRC",
        version = "v2.0-public"
      )
    )

})
