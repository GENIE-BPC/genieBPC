# internal table of ncol and nrow for each data release
# used for tests
# requires update for each data release
data_releases_expected_size <-  read.csv(here::here("data-raw/data_releases_expected_size.csv"))

usethis::use_data(data_releases_expected_size,
                  internal = TRUE, overwrite = TRUE)
