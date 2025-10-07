regimen_abbreviations <- read.csv(here::here("data-raw/regimen_abbreviations.csv"))

usethis::use_data(regimen_abbreviations, overwrite = TRUE)
