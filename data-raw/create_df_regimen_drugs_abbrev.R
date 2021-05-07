#' Synapse table IDs
#'
#' A dataset containing the Synapse table IDs for each clinical dataset in GENIE BPC.
#'
#' @format A lookup table for Synapse clinical data table IDs:
#' \describe{
#'   \item{regimen_drugs}{Names of cancer-directed drugs in regimen (up to 5)}
#'   \item{abbreviation}{Common clinical term for combination of drugs in regimen}
#'   ...
#' }

regimen_drugs_lookup <- read.csv("C:/Users/laveryj/Desktop/GenieBPC/data-raw/regimen_drugs_abbrev.csv")

usethis::use_data(regimen_drugs_lookup, overwrite = TRUE)
