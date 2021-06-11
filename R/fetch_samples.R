#' fetch_samples
#'
#' Add samples for each patients of the cohort of interest
#' @param cohort GENIE BPC Project cancer. Must be one of "NSCLC" or "CRC".
#' @param cohort_object output object of the create_cohort function.
#' @return returns the cohort object list inputted with an additional dataset named "samples_data".
#' @export
#' @author Axel Martin
#' @import
#' dplyr
#' dtplyr
#' tibble

fetch_samples <- function(cohort, cohort_object){

  if(missing(cohort))
    stop("You must provide a cohort name ('NSCLC' or 'CRC') function in the `cohort` argument.")
  cohort_temp <- cohort

  if(missing(cohort_object))
    stop("You must provide an object outputted by the create_cohort function in the `cohort_object` argument.")
  # keep based on patient ID + CA seq of interest #
  ids <- unique(paste0(cohort_object$cohort_ca_dx$record_id,"_",cohort_object$cohort_ca_dx$ca_seq))

  sample_temp <- get(paste0("cpt_", cohort_temp)) %>%
    rowwise() %>%
    mutate(IDs = paste0(record_id,"_",ca_seq)) %>%
    ungroup() %>%
    filter(IDs %in% ids) %>%
    select(record_id,cpt_genie_sample_id,institution,ca_seq,cpt_seq_assay_id,
           cpt_sample_type,cpt_oncotree_code,dx_cpt_rep_days,dx_cpt_rep_mos)

  # dim(get(paste0("cpt_", cohort_temp)) %>%
  #       select(record_id,cpt_genie_sample_id,institution,ca_seq,cpt_seq_assay_id,
  #              cpt_sample_type,cpt_oncotree_code,dx_cpt_rep_days,dx_cpt_rep_mos) %>%
  #   filter(record_id %in% unique(cohort_object$cohort_ca_dx$record_id)) )

  cohort_object$samples_data <- sample_temp
  return(cohort_object)

  # full_join(
  #   dat,
  #   clin_dat %>%
  #     rename(time_dob_sequencing = Time..years..from.DOB.to.NGS.sequencing.report) %>%
  #     mutate(Oncotree.Code = as.character(Oncotree.Code),
  #            Sequence.Assay.ID = as.character(Sequence.Assay.ID),
  #            time_dob_sequencing = as.numeric(as.character(time_dob_sequencing))) %>%
  #     filter(Patient.Identifier %in% cohort_object$ID) %>%
  #     select(Patient.Identifier, X.Sample.Identifier, Sequence.Assay.ID,
  #            Oncotree.Code,time_dob_sequencing,
  #            Sample.Type) %>%
  #     rename(ID = Patient.Identifier, sample_ID = X.Sample.Identifier),
  #   by = "ID") %>%
  #   mutate(regimen_time = as.numeric(as.character(dob)),
  #          time_regimen_sequencing = time_dob_sequencing*12 -
  #            regimen_time/30.4375)
}
