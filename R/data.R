#' List of Drug Regimen Names by Cohort
#'
#' A dataset containing the cancer-directed drug names and their synonyms.
#'
#' @format A table for cancer-directed drug names associated with each
#' cancer cohort:
#' \describe{
#'   \item{cohort}{GENIE BPC Project cancer. Must be one of "NSCLC"
#'   (non-small cell lung cancer), "CRC" (colorectal cancer), or
#'   "BrCa" (breast cancer). Future cohorts will include "PANC"
#'   (pancreatic cancer), "Prostate" (prostate cancer), and "BLADDER"
#'   (bladder cancer).}
#'   \item{drug_name}{Name of generic/ingredient cancer-directed drug}
#'   \item{drug_name_full}{Name of generic/ingredient cancer-directed drug
#'   with associated synonyms in parentheses}
#'   ...
#' }
"drug_regimen_list"


#' Genomic Panels Included in GENIE BPC Data
#'
#' A dataset containing the name, assay identifier, and number of genes
#' in each next-generation sequencing targeted panel included in GENIE BPC.
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{Sequence.Assay.ID}{Next-generation
#'   sequencing targeted panel assay identifier}
#'   \item{Panel}{Panel name}
#'   \item{Genes}{Number of genes included}
#'   ...
#' }
"genie_panels"


#' List of Drug Regimen Abbreviations
#'
#' A dataset containing the cancer-directed
#' drug regimens and their common abbreviations
#'
#' @format A table for cancer-directed drug regimens and
#' their common abbreviations
#' \describe{
#'   \item{regimen_drugs}{List of all drugs in the regimen}
#'   \item{abbreviation}{Common name of drug regimen, e.g. FOLFOX}
#'   ...
#' }
"regimen_abbreviations"


#' Synapse table IDs
#'
#' A dataset containing the Synapse table IDs for
#' each clinical dataset in GENIE BPC.
#'
#' @format A lookup table for Synapse clinical data table IDs:
#' \describe{
#'   \item{cohort}{GENIE BPC Project Cohort}
#'   \item{df}{Clinical dataset}
#'   \item{version}{Release version}
#'   \item{synapse_id}{Synapse table ID for each dataset}
#'   \item{release_date}{Month and year of data release}
#'   ...
#' }
#' @source \url{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}
"synapse_tables"


#' Simulated fake synapse data for function examples and tests
#'
#' A named list of simulated NSCLC data
#'
#' @format A list of cohort data frames
#' \describe{
#'     \item{pt_char_NSCLC}{Patient characteristic data.frame}
#'     \item{ca_dx_index_NSCLC}{Cancer Dx data.frame}
#'     \item{ca_drugs_NSCLC}{Drug data.frame}
#'     \item{cpt_NSCLC}{CPT data.frame}
#' }
"nsclc_test_data"
