% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{drug_regimen_list}
\alias{drug_regimen_list}
\title{List of Drug Regimen Names by Cohort}
\format{
A table for cancer-directed drug names associated with each
cancer cohort:
\describe{
  \item{cohort}{GENIE BPC Project cancer. Must be one of "NSCLC"
  (non-small cell lung cancer), "CRC" (colorectal cancer), or
  "BrCa" (breast cancer). Future cohorts will include "PANC"
  (pancreatic cancer), "Prostate" (prostate cancer), and "BLADDER"
  (bladder cancer).}
  \item{drug_name}{Name of generic/ingredient cancer-directed drug}
  \item{drug_name_full}{Name of generic/ingredient cancer-directed drug
  with associated synonyms in parentheses}
  ...
}
}
\usage{
drug_regimen_list
}
\description{
A dataset containing the cancer-directed drug names and their synonyms.
}
\keyword{datasets}
