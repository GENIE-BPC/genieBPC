#' Bind together a list of synapse data objects
#'
#' @param data_list a list of synapse data objects
#'
#' @return a list of GENIE dataframes (pt_char, ca_dx, etc.) that contains
#'         data from more than one cohort
#'
#' @keywords internal
#' @export
#'

.bind_genie_data <- function(data_list) {
  dataset_cohorts <- c("cohort_pt_char", "cohort_ca_dx",
                       "cohort_ca_drugs", "cohort_ca_dx_non_index",
                                     "cohort_prissmm_pathology", "cohort_prissmm_imaging",
                                     "cohort_prissmm_md", "cohort_tumor_marker",
                                     "cohort_ngs", "cohort_mutations_extended",
                                     "cohort_cna", "cohort_fusions")

save <-  imap((dataset_cohorts), \(y, idx){
    map(data_list, function(x) {

      # skip rad/tm if in cohorts without it
      if((unique(x[[1]]$cohort) %in% c(
        "BrCa", "CRC", "NSCLC"
      ) & y == "cohort_ca_radtx") | (unique(x[[1]]$cohort %in%
                                       c("BLADDER", "NSCLC") & y == "cohort_tumor_marker"))){
        NULL
      } else {
        # grab data frame with correct name
        z <- x[names(x) == y][[1]]


      # reassign these variables to character because for
      # one cohort or another there is a character option
      # such as "Not applicable" written out as a word
      if (y %in% c("cohort_ca_dx_non_index", "cohort_ca_dx")) {
        z <- z %>%
          mutate(across(any_of(c("naaccr_laterality_cd",
                          "naaccr_tnm_path_desc")), ~as.character(.)))
      }

      if (y == "cohort_prissmm_pathology") {
        z <- z %>%
          mutate(across(c(pdl1_iclrange,
                          pdl1_iclrange_2,
                          pdl1_icurange,
                          pdl1_icurange_2,
                          pdl1_tcurange,
                          pdl1_lcpsrange), ~as.character(.)))
      }

      if ("release_version" %in% names(z)){
      z <- z %>%
          mutate(release_version = as.character(release_version))
      }

      if ("cpt_seq_date" %in% names(z)){
        z <- z %>%
          mutate(cpt_seq_date = as.numeric(cpt_seq_date))%>%
          base::suppressWarnings()
      }
        z
      }


    }) %>%
    # drop any NULL lists
    compact()%>%
    # full join all datasets with the same name
    reduce(full_join)%>%
    suppressMessages()
  })

# label the datsets accordingly
names(save) <- dataset_cohorts

save

}

