#' opt_samples
#'
#' Get the best samples for analysis following several user define criterions.
#' @param samples_object output object of the fectch_samples function.
#' @param sample_type character specifying which type of sample to prioritize, options are "Primary" and "Metastasis".
#' Default is either.
#' @param histology character vector specifying which sample histologies to keep. See "Oncotree.Code" column
#' of samples_object argument above to get options.

opt_samples <- function(samples_object, sample_type = c("Primary","Metastasis"),histology){

  samples_object <- samples_object %>%
    filter(Oncotree.Code %in% histology)

  ### Find patients that had duplicated samples ###
  dup_samples <- as.character(unlist(samples_object %>%
                                       group_by(ID) %>%
                                       summarise(N_samples = length(unique(sample_ID))) %>%
                                       filter(N_samples > 1) %>%
                                       select(ID)))

  rm <- unlist(lapply(dup_samples, function(x){
    temp <- samples_object %>%
      filter(ID == x)

    temp_hist <- temp %>%
      filter(Sample.Type %in% sample_type)

    if(nrow(temp_hist) > 0){
      # find sample report closest to regimen start #
      index_min <- which.min(abs(temp_hist$time_regimen_sequencing))
      time_min <- temp_hist$time_regimen_sequencing[index_min]
      temp_hist_min <- temp_hist %>%
        filter(time_regimen_sequencing == time_min)
      if(nrow(temp_hist_min) == 1){
        return(as.character(unlist(temp %>%
                                     filter(sample_ID != temp_hist_min$sample_ID) %>%
                                     # filter(time_regimen_sequencing != time_min) %>%
                                     select(sample_ID))))
      }
      else{
        # if one sample has larger sample keep that one #
        plats_temps <- as.character(unique(temp_hist_min$Sequence.Assay.ID))
        plat_keep <- plats_temps[which.max(as.numeric(unlist(regmatches(plats_temps, gregexpr("[[:digit:]]+", plats_temps)))))]
        # keep only samples with the larger platform #
        temp_hist_min_plat <- temp_hist_min %>%
          filter(Sequence.Assay.ID == plat_keep)
        if(nrow(temp_hist_min_plat) == 1){
          return(as.character(unlist(temp_hist %>%
                                       filter(sample_ID != temp_hist_min_plat$sample_ID) %>%
                                       select(sample_ID))))
        }
        else{
          # if no platform difference pick the one that was taken the latest (?) #
          keep <- as.character(temp_hist_min_plat$sample_ID[which.max(as.numeric(unlist(lapply(regmatches(temp_hist_min_plat$sample_ID, gregexpr("[[:digit:]]+", temp_hist_min_plat$sample_ID)),
                                                                                               function(y){
                                                                                                 paste0(y, collapse = "")
                                                                                               }))))])
          return(as.character(unlist(temp %>%
                                       filter(sample_ID != keep) %>%
                                       select(sample_ID))))
        }
      }
    }

    # for samples without any priority samples #
    else{
      # find sample report closest to regimen start #
      index_min <- which.min(abs(temp$time_regimen_sequencing))
      time_min <- temp$time_regimen_sequencing[index_min]
      temp_min <- temp %>%
        filter(time_regimen_sequencing == time_min)
      if(nrow(temp_min) == 1){
        return(as.character(unlist(temp %>%
                                     filter(time_regimen_sequencing != time_min) %>%
                                     select(sample_ID))))
      }
      else{
        # if one sample has larger sample keep that one #
        plats_temps <- as.character(unique(temp_min$Sequence.Assay.ID))
        plat_keep <- plats_temps[which.max(as.numeric(unlist(regmatches(plats_temps, gregexpr("[[:digit:]]+", plats_temps)))))]
        # keep only samples with the larger platform #
        temp_min_plat <- temp_min %>%
          filter(Sequence.Assay.ID == plat_keep)
        if(nrow(temp_min_plat) == 1){
          return(as.character(unlist(temp %>%
                                       filter(sample_ID != temp_hist_min_plat$sample_ID) %>%
                                       select(sample_ID))))
        }
        else{
          # if no platform difference pick the one that was taken the latest (?) #
          keep <- as.character(temp_min_plat$sample_ID[which.max(as.numeric(unlist(lapply(regmatches(temp_min_plat$sample_ID, gregexpr("[[:digit:]]+", temp_min_plat$sample_ID)),
                                                                                          function(y){
                                                                                            paste0(y, collapse = "")
                                                                                          }))))])
          return(as.character(unlist(temp %>%
                                       filter(sample_ID != keep) %>%
                                       select(sample_ID))))
        }
      }
    }

  }))

  # rm duplicated samples #
  samples_object_final <- samples_object %>%
    filter(!(sample_ID %in% rm))

  return(samples_object_final)
}
