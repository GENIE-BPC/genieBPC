#' select_unique_ngs
#'
#' Get a unique next generation sequencing sample
#' for each patient for analysis following several
#' user define criterions.
#' @param data_cohort output object of the create_analytic_cohort function.
#' @param oncotree_code character vector specifying which sample
#' OncoTree codes to keep. See "cpt_oncotree_code" column
#' of data_cohort argument above to get options.
#' @param sample_type character specifying which type of genomic sample
#' to prioritize, options are "Primary", "Local" and "Metastasis".
#' Default is to not select a NGS sample based on the sample type.
#' @param min_max_time character specifying if the first or last genomic
#' sample recorded should be kept.
#' Options are "min" (first) and "max" (last).
#'
#' @return returns the 'cohort_ngs' object of the create_analytic_cohort
#' with an unique genomic samples taken from each patients.
#' @export
#'
#' @examples
#' if(genieBPC:::.check_synapse_login() == TRUE){
#' #' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage
#' # IV NSCLC of histology adenocarcinoma
#' nsclc_data <- pull_data_synapse("NSCLC", version = "2.1-consortium")
#' out <- create_analytic_cohort(cohort = "NSCLC",
#' data_synapse = nsclc_data, stage_dx = c("Stage IV"),
#' histology = "Adenocarcinoma")
#' samples_data <- select_unique_ngs(data_cohort = out$cohort_ngs)
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who
#' # received Cisplatin, Pemetrexed Disodium or Cisplatin,
#' # Etoposide as their first drug regimen
#' out <- create_analytic_cohort(cohort = "NSCLC",
#' data_synapse = nsclc_data,
#' regimen_drugs = c("Cisplatin, Pemetrexed Disodium",
#' "Cisplatin, Etoposide"), regimen_order = 1,
#' regimen_order_type = "within regimen")
#' samples_data <- select_unique_ngs(data_cohort = out$cohort_ngs,
#' oncotree_code = "LUAD", sample_type = "Metastasis",min_max_time = "max")
#' }
#' @import
#' dplyr
#' dtplyr
#' tibble
select_unique_ngs <- function(data_cohort, oncotree_code = NULL,
                              sample_type = NULL, min_max_time = NULL) {

  # perform checks #
  if (missing(data_cohort)) {
    stop("The 'data_cohort' argument is needed to perform this process.
         'data_cohort' is the output created by the 'fetch_samples' function,
         or the object 'cohort_cpt' from the create_analytic_cohort()
         function.")
  }
  # if(sum(grepl("samples_data",names(data_cohort))) != 1)
  #   stop("The 'data_cohort' input did not contain the 'samples_data' object.
  # Is 'data_cohort' input an output of the 'fetch_samples' function?")
  if (is.null(oncotree_code) && is.null(sample_type) && is.null(min_max_time)) {
    warning("None of the optimization arguments were specified. The sample with
            the largest panel size will be returned. In the case of ties a
            random sample will be returned.")
  }
  if (!is.null(oncotree_code) && sum(data_cohort$cpt_oncotree_code %in%
                                     oncotree_code) == 0) {
    warning("The OncoTree code inputted does not exist in the data
            and thus will be ignored.")
    oncotree_code <- NULL
  }
  if (!is.null(min_max_time) && (!(min_max_time %in% c("min", "max")) ||
      length(min_max_time) > 1)) {
    stop("The 'min_max_time' argument should be either 'min' or 'max'
         (only one of the two).")
  }
  if (!is.null(sample_type) &&
      (length(sample_type) > 1 ||
       !(stringr::str_to_lower(sample_type) %in%
         c("primary", "local", "metastasis"))))
    {
    stop("Please input a single sample of type of interest out of 'Primary',
         'Local' or 'Metastasis'")
  }

  # samples_data <- data_cohort$samples_data
  # we perform the optimization only for patients that have multiple samples #
  ### Find patients that had duplicated samples ###
  dup_samples <- as.character(unlist(data_cohort %>%
    group_by(.data$record_id) %>%
    summarise(N_samples = length(unique(.data$cpt_genie_sample_id))) %>%
    filter(.data$N_samples > 1) %>%
    select(.data$record_id)))
  # data_cohort %>%
  #   group_by(record_id) %>%
  #   summarise(N_samples = length(unique(cpt_genie_sample_id))) %>%
  #   filter(record_id == "GENIE-DFCI-004022")
  solved_dups <- as.data.frame(
    do.call(
      "rbind",
      lapply(dup_samples, function(x) {
        # print(x)
        temp <- data_cohort %>%
          filter(.data$record_id == x)

        # deal with sample site #
        if (!is.null(oncotree_code) &&
            (sum(temp$cpt_oncotree_code %in% oncotree_code) > 1)) {
          temp <- temp %>%
            filter(.data$cpt_oncotree_code %in% oncotree_code)
        }
        if (!is.null(oncotree_code)
            && (sum(temp$cpt_oncotree_code %in% oncotree_code) == 0)) {
          warning(paste0("Patient ", x, " did not have any sample of source: ",
                         oncotree_code))
        }

        # deal with sample type #
        if (!is.null(sample_type) &&
            (sum(grepl(sample_type, temp$cpt_sample_type,
                       ignore.case = TRUE)) > 0)) {
          temp <- temp[grepl(sample_type, temp$sample_type,
                             ignore.case = TRUE), ]
        }
        if (!is.null(sample_type) &&
            (sum(grepl(sample_type, temp$cpt_sample_type,
                       ignore.case = TRUE)) == 0)) {
          warning(paste0("Patient ", x, " did not have any sample of source: ",
                         sample_type))
        }

        # deal with time #
        if (!is.null(min_max_time)) {
          if (min_max_time == "min") {
            temp <- temp %>%
              filter(.data$dx_cpt_rep_days == min(.data$dx_cpt_rep_days))
          }
          if (min_max_time == "max") {
            temp <- temp %>%
              filter(.data$dx_cpt_rep_days == max(.data$dx_cpt_rep_days))
          }
        }

        # If there are still multiple samples, then
        # select the sample with largest panel number
        if (nrow(temp) > 1) {
          temp <- temp %>%
            filter(.data$cpt_seq_assay_id %in%
                     genieBPC::genie_panels$Sequence.Assay.ID) %>%
            rowwise() %>%
            mutate(panel_size =
                     genieBPC::genie_panels[match(
                       .data$cpt_seq_assay_id,
                       genieBPC::genie_panels$Sequence.Assay.ID),
                                  "Genes"]) %>%
            ungroup() %>%
            filter(.data$panel_size == max(.data$panel_size)) %>%
            select(-one_of("panel_size"))
        }

        # If somehow there are still multiple possible samples, then
        # pick one at random
        if (nrow(temp) > 1) {
          warning(paste0("Patient ", x, " still had multiple possible
                         samples based on the selected arguments,
                         a sample was selected at random."))
          # Set seed so this is reproducible #
          set.seed(210793)
          temp <- temp[sample(seq_along(temp[,1]), size = 1), ]
        }

        return(temp)
      })
    )
  )

  # quick check #
  # nrow(solved_dups) == length(dup_samples)

  # remove all patients with duplicates and add back their selected samples #
  samples_data_final <- rbind(
    data_cohort %>%
      filter(!(.data$record_id %in% dup_samples)),
    solved_dups
  )

  return(samples_data_final)
}
