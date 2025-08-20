cohort_institution <- tibble::tribble(
  ~cohort, ~institution,
  "NSCLC", "MSK",
  "NSCLC", "DFCI",
  "NSCLC", "UHN",
  "NSCLC", "VICC",
  "CRC", "MSK",
  "CRC", "DFCI",
  "CRC", "VICC",
  "BrCa", "MSK",
  "BrCa", "DFCI",
  "BrCa", "VICC",
  "PANC", "MSK",
  "PANC", "DFCI",
  "PANC", "UHN",
  "PANC", "VICC",
  "Prostate", "MSK",
  "Prostate", "DFCI",
  "Prostate", "UHN",
  "Prostate", "VICC",
  "BLADDER", "MSK",
  "BLADDER", "DFCI",
  "BLADDER", "UHN",
  "BLADDER", "VICC"
)

usethis::use_data(cohort_institution, internal = FALSE, overwrite = TRUE)
