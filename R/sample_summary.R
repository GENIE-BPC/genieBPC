#' sample_summary
#'
#' Creates a summary of the samples retrieved.
#' @param samples_object output object of the fectch_samples function.
#' @import
#' dplyr
#' dtplyr
#' tibble

sample_summary <- function(samples_object){
  samples_object %>%
    group_by(Center, regimen, line) %>%
    summarise(N_samples = n(),
              N_patient = length(unique(ID)),
              `Median time1` = paste0(round(median(time_regimen_sequencing, na.rm = T),2),
                                      " (",round(min(time_regimen_sequencing,na.rm = T),2),",",
                                      round(max(time_regimen_sequencing,na.rm = T),2),")"
              ),
              Primary = paste0(round(sum(Sample.Type == "Primary", na.rm = T)/N_samples*100,1),"%"),
              Metastasis = paste0(round(sum(Sample.Type == "Metastasis", na.rm = T)/N_samples*100,1),"%"),
              `Tissue Summary` = paste0(names(sort(summary(as.factor(Oncotree.Code)), decreasing = T))
                                        ,"=",sort(summary(as.factor(Oncotree.Code)), decreasing = T),
                                        collapse = ", ")
    )
}
