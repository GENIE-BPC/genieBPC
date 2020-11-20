#' cohort_summary
#'
#' Creates a summary of the cohort created.
#' @param cohort_object output object of the create_cohort function.
#' @import
#' dplyr
#' dtplyr
#' tibble

cohort_summary <- function(cohort_object){
  dat %>%
    group_by(Center, regimen, line) %>%
    summarise(N = n(),
              `Median OS` = paste0(round(median(os_time, na.rm = T),2),
                                   " (",round(min(os_time,na.rm = T),2),",",
                                   round(max(os_time,na.rm = T),2),")"
              ),
              `Event rate OS` = paste0(round(sum(os_status, na.rm = T)/N*100,1),"%"),

              `Median PFS I` = paste0(round(median(pfs_I_time, na.rm = T),2),
                                      " (",round(min(pfs_I_time,na.rm = T),2),",",
                                      round(max(pfs_I_time,na.rm = T),2),")"
              ),
              `Event rate PFS I` = paste0(round(sum(pfs_I_status, na.rm = T)/N*100,1),"%"),

              `Median PFS M` = paste0(round(median(pfs_M_time, na.rm = T),2),
                                      " (",round(min(pfs_I_time,na.rm = T),2),",",
                                      round(max(pfs_I_time,na.rm = T),2),")"
              ),
              `Event rate PFS M` = paste0(round(sum(pfs_M_status, na.rm = T)/N*100,1),"%")
    ) %>%
    arrange(-line, -N)
}
