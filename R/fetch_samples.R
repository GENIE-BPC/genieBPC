#' fetch_samples
#'
#' Add samples for each patients of the cohort of interest
#' @param clin_dat dataframe containing the sample information to be matched with the patients from the cohort of interest.
#' eg: data_clinical_sample.txt.
#' @param cohort_object output object of the create_cohort function.
#' @import
#' dplyr
#' dtplyr
#' tibble

fetch_samples <- function(clin_dat, cohort_object){
  full_join(
    dat,
    clin_dat %>%
      filter(Patient.Identifier %in% cohort_object$ID) %>%
      select(Patient.Identifier, X.Sample.Identifier, Sequence.Assay.ID,
             Oncotree.Code,Time..years..from.DOB.to.NGS.sequencing.report,
             Sample.Type) %>%
      rename(ID = Patient.Identifier, sample_ID = X.Sample.Identifier),
    by = "ID")
}
