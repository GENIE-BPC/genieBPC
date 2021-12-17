#' List of Drug Regimen Names by Cohort
#'
#' A dataset containing the cancer-directed drug names and their synonyms.
#'
#' @format A table for cancer-directed drug names associated with each
#' cancer cohort:
#' \describe{
#'   \item{cohort}{GENIE BPC Project cancer. Must be one of "NSCLC"
#'   (non-small cell lung cancer) or "CRC" (colorectal cancer).
#'   Future cohorts will include "BrCa" (breast cancer), "PANC"
#'   (pancreatic cancer), "Prostate" (prostate cancer).}
#'   \item{drug_name}{Name of generic/ingredient cancer-directed drug}
#'   \item{drug_name_full}{Name of generic/ingredient cancer-directed drug
#'   with associated synonyms in parentheses}
#'   ...
#' }
"drug_regimen_list"
