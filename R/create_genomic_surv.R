#' create_genomic_surv
#'
#' Create a time to event dataset with their corresponding features
#' @param samples_object output object of the opt_samples function (also works with fetch_samples function).
#' @param gen_dat output of binmat() (from package gnomeR) created from the corresponding samples.
#' @param surv_type select a time to event type of interest. Options are "os", "pfs_i" or "pfs_m".
#' Default is "pfs_m".
#' @param ignore_pb_LT boolean specifying if cases with time1 > time2 should be treated as no delayed truncation.
#' Default is TRUE if event type is PFS, FALSE if event type is OS.
#' @param complete_cases boolean specifying if only complete cases should be kept. Default is TRUE.
#' @export
#' @import
#' dplyr
#' dtplyr
#' tibble

create_genomic_surv <- function(samples_object, gen_dat, surv_type = "pfs_m",
                                ignore_pb_LT = ifelse(surv_type %in% "os", FALSE, TRUE),
                                complete_cases = TRUE){
  # select specific time point of interest #
  if(surv_type == "os"){
    keep <- c("time_regimen_sequencing", "os_time", "os_status")
    samples_object <- samples_object %>%
      select(c("sample_ID",keep)) %>%
      rename(time1 = time_regimen_sequencing, time2 = os_time, status = os_status)
  }
  if(surv_type == "pfs_i"){
    keep <- c("time_regimen_sequencing", "pfs_I_time", "pfs_I_status")
    samples_object <- samples_object %>%
      select(c("sample_ID",keep)) %>%
      rename(time1 = time_regimen_sequencing, time2 = pfs_I_time, status = pfs_I_status)
  }
  if(surv_type == "pfs_m"){
    keep <- c("time_regimen_sequencing", "pfs_M_time", "pfs_M_status")
    samples_object <- samples_object %>%
      select(c("sample_ID",keep)) %>%
      rename(time1 = time_regimen_sequencing, time2 = pfs_M_time, status = pfs_M_status)
  }

  # join data #
  full_dat <- full_join(
    samples_object %>%
      mutate(time1 = ifelse(time1 < 0, 0, time1)),
    gen_dat %>%
      tibble::rownames_to_column("sample_ID"),
    by = "sample_ID"
  ) %>%
    tibble::column_to_rownames("sample_ID")

  if(ignore_pb_LT)
    full_dat <- full_dat %>%
      mutate(time1 = ifelse(time1 >= time2, 0, time1))

  if(complete_cases)
    full_dat <- full_dat[stats::complete.cases(full_dat),]

  full_dat <- full_dat %>%
    filter(time1 < time2)
  return(full_dat)
}


