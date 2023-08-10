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


#' 'Synapse' table IDs
#'
#' A dataset containing the 'Synapse' table IDs for
#' each dataset in GENIE BPC.
#'
#' @format A lookup table for 'Synapse' data table IDs:
#' \describe{
#'   \item{cohort}{GENIE BPC Project Cohort}
#'   \item{release_date}{Month and year of data release}
#'   \item{version}{Release version}
#'   \item{df}{Clinical, Genomic, or Metadata Dataset}
#'   \item{synapse_id}{'Synapse' table ID for each dataset}
#'   ...
#' }
#' @source \url{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}
"synapse_tables"


#' Simulated fake GENIE BPC data for function examples and tests
#'
#' A named list of simulated NSCLC clinical and genomic data
#'
#' @format A list of data frames
#' \describe{
#'     \item{pt_char}{Patient characteristic data.frame}
#'     \item{ca_dx_index}{Index cancer diagnosis data.frame}
#'     \item{ca_dx_non_index}{Non-index cancer diagnosis data.frame}
#'     \item{ca_drugs}{Cancer directed-regimen data.frame}
#'     \item{prissmm_imaging}{PRISSMM Imaging report data.frame}
#'     \item{prissmm_pathology}{PRISSMM Pathology report data.frame}
#'     \item{prissmm_md}{PRISSMM medical oncologist report data.frame}
#'     \item{cpt}{Cancer Panel Test (CPT)/Next Generation Sequencing (NGS) data.frame}
#'     \item{mutations_extended}{Mutations data.frame}
#'     \item{fusions}{Fusions data.frame}
#'     \item{cpt}{Copy Number Alteration (CNA) data.frame}
#' }
"nsclc_test_data"
