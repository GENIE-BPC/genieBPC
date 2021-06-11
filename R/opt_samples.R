#' opt_samples
#'
#' Get the best samples for analysis following several user define criterions.
#' @param samples_object output object of the fetch_samples function.
#' @param histology character vector specifying which sample histologies to keep. See "cpt_oncotree_code" column
#' of samples_object argument above to get options.
#' @param sample_type character specifying which type of sample to prioritize, options are "Primary" and "Metastasis".
#' Default is either.
#' @param min_max_time character specifying if the first or last sample recorded should be kept.
#' Options are "min" (first) and "max" (last).
#'
#' @return returns the sample object list inputted with an additional dataset named "samples_data".
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' # Create a cohort of all patients with stage IV NSCLC of histology adenocarcinoma
#' out <- create_cohort(cohort = "NSCLC",
#'      stage_dx = c("Stage IV"),
#'      ca_hist_adeno_squamous = "Adenocarcinoma")
#' samples_data <- fetch_samples(cohort = "NSCLC", cohort_object = out)
#' opt_samples <- opt_samples(samples_object = samples_data)
#' # Example 2 ----------------------------------
#' # Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
#' out <- create_cohort(cohort = "NSCLC",
#'      regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin, Etoposide"),
#'      regimen_order = 1,
#'      regimen_order_type = "within regimen")
#' samples_data <- fetch_samples(cohort = "NSCLC", cohort_object = out)
#' opt_samples <- opt_samples(samples_object = samples_data, histology = "LUAD", sample_type = "Metastasis",min_max_time = "max")
#' @import
#' dplyr
#' dtplyr
#' tibble
opt_samples <- function(samples_object, histology = NULL, sample_type = NULL, min_max_time = NULL){

  # perform checks #
  if(missing(samples_object))
    stop("The 'samples_object' argument is needed to perform this process. 'samples_object' is the output created by the 'fetch_samples' function.")
  if(sum(grepl("samples_data",names(samples_object))) != 1)
    stop("The 'samples_object' input did not contain the 'samples_data' object. Is 'samples_object' input an output of the 'fetch_samples' function?")
  if(is.null(histology) && is.null(sample_type) && is.null(min_max_time))
    warning("None of the optimization arguments were specified. The sample with the largest panel size will be returned. In the case of ties a random sample will be returned.")
  if(!is.null(histology) && sum(samples_data$samples_data$cpt_oncotree_code %in% histology) == 0){
    warning("The histology inputted do not exist in the samples data and thus will be ignored.")
    histology <- NULL
  }
  if(!is.null(min_max_time) && !(min_max_time %in% c("min","max")) && length(min_max_time) > 1){
    stop("The 'min_max_time' argument should be either 'min' or 'max' (only one of the two).")
  }


  samples_data <- samples_object$samples_data
  # we perform the optimization only for patients that have multiple samples #
  ### Find patients that had duplicated samples ###
  dup_samples <- as.character(unlist(samples_data %>%
                                       group_by(record_id) %>%
                                       summarise(N_samples = length(unique(cpt_genie_sample_id))) %>%
                                       filter(N_samples > 1) %>%
                                       select(record_id)))
  # samples_data %>%
  #   group_by(record_id) %>%
  #   summarise(N_samples = length(unique(cpt_genie_sample_id))) %>%
  #   filter(record_id == "GENIE-MSK-P-0012203")
  solved_dups <- as.data.frame(
    do.call("rbind",
            lapply(dup_samples, function(x){
              # print(x)
              temp <- samples_data %>%
                filter(record_id == x)

              # deal with sample site #
              if(!is.null(histology) && (sum(temp$cpt_oncotree_code %in% histology) > 1)){
                temp <- temp %>%
                  filter(cpt_oncotree_code %in% histology)
              }
              if(!is.null(histology) && (sum(temp$cpt_oncotree_code %in% histology) == 0))
                warning(paste0("Patient ",x," did not have any sample of source: ", histology))

              # deal with sample type #
              if(!is.null(sample_type) && (sum(grepl(sample_type,temp$cpt_sample_type, ignore.case = T)) > 1)){
                temp <- temp %>%
                  filter(grepl(sample_type,temp$cpt_sample_type, ignore.case = T))
              }
              if(!is.null(sample_type) && (sum(grepl(sample_type,temp$cpt_sample_type, ignore.case = T)) == 0))
                warning(paste0("Patient ",x," did not have any sample of source: ", sample_type))

              # deal with time #
              if(!is.null(min_max_time)){
                if(min_max_time == "min")
                  temp <- temp %>%
                    filter(dx_cpt_rep_days == min(dx_cpt_rep_days))
                if(min_max_time == "max")
                  temp <- temp %>%
                    filter(dx_cpt_rep_days == max(dx_cpt_rep_days))
              }

              # If there are still multiple samples select the sample with largest panel #
              if(nrow(temp) > 1){
                temp <- temp %>%
                  filter(cpt_seq_assay_id %in% panel_names$Sequence.Assay.ID) %>%
                  rowwise() %>%
                  mutate(Panel_size = length(unique(get(panel_names[match(cpt_seq_assay_id, panel_names$Sequence.Assay.ID),"Panel"])))) %>%
                  ungroup() %>%
                  filter(Panel_size == max(Panel_size)) %>%
                  select(-one_of("Panel_size"))
              }

              # If somehow there is still multiple possible samples pick one at random... #
              if(nrow(temp) > 1){
                warning(paste0("Patient ",x, " still had multiple possible samples based on the selected arguments, a sample was selected at random."))
                # Set seed so this is reproduceable #
                set.seed(210793)
                temp <- temp[sample(1:nrow(temp),size = 1),]
              }

              return(temp)
            })
    )
  )

  # quick check #
  # nrow(solved_dups) == length(dup_samples)

  # remove all patients with duplicates and add back their selected samples #
  samples_data_final <- rbind(
    samples_data %>%
      filter(!(record_id %in% dup_samples)),
    solved_dups)

  samples_object$opt_samples_data <- samples_data_final
  return(samples_object)


  # samples_object <- samples_object %>%
  #   filter(Oncotree.Code %in% histology)
  #
  # ### Find patients that had duplicated samples ###
  # dup_samples <- as.character(unlist(samples_object %>%
  #                                      group_by(ID) %>%
  #                                      summarise(N_samples = length(unique(sample_ID))) %>%
  #                                      filter(N_samples > 1) %>%
  #                                      select(ID)))
  #
  # rm <- unlist(lapply(dup_samples, function(x){
  #   temp <- samples_object %>%
  #     filter(ID == x)
  #
  #   temp_hist <- temp %>%
  #     filter(Sample.Type %in% sample_type)
  #
  #   if(nrow(temp_hist) > 0){
  #     # find sample report closest to regimen start #
  #     index_min <- which.min(abs(temp_hist$time_regimen_sequencing))
  #     time_min <- temp_hist$time_regimen_sequencing[index_min]
  #     temp_hist_min <- temp_hist %>%
  #       filter(time_regimen_sequencing == time_min)
  #     if(nrow(temp_hist_min) == 1){
  #       return(as.character(unlist(temp %>%
  #                                    filter(sample_ID != temp_hist_min$sample_ID) %>%
  #                                    # filter(time_regimen_sequencing != time_min) %>%
  #                                    select(sample_ID))))
  #     }
  #     else{
  #       # if one sample has larger sample keep that one #
  #       plats_temps <- as.character(unique(temp_hist_min$Sequence.Assay.ID))
  #       plat_keep <- plats_temps[which.max(as.numeric(unlist(regmatches(plats_temps, gregexpr("[[:digit:]]+", plats_temps)))))]
  #       # keep only samples with the larger platform #
  #       temp_hist_min_plat <- temp_hist_min %>%
  #         filter(Sequence.Assay.ID == plat_keep)
  #       if(nrow(temp_hist_min_plat) == 1){
  #         return(as.character(unlist(temp_hist %>%
  #                                      filter(sample_ID != temp_hist_min_plat$sample_ID) %>%
  #                                      select(sample_ID))))
  #       }
  #       else{
  #         # if no platform difference pick the one that was taken the latest (?) #
  #         keep <- as.character(temp_hist_min_plat$sample_ID[which.max(as.numeric(unlist(lapply(regmatches(temp_hist_min_plat$sample_ID, gregexpr("[[:digit:]]+", temp_hist_min_plat$sample_ID)),
  #                                                                                              function(y){
  #                                                                                                paste0(y, collapse = "")
  #                                                                                              }))))])
  #         return(as.character(unlist(temp %>%
  #                                      filter(sample_ID != keep) %>%
  #                                      select(sample_ID))))
  #       }
  #     }
  #   }
  #
  #   # for samples without any priority samples #
  #   else{
  #     # find sample report closest to regimen start #
  #     index_min <- which.min(abs(temp$time_regimen_sequencing))
  #     time_min <- temp$time_regimen_sequencing[index_min]
  #     temp_min <- temp %>%
  #       filter(time_regimen_sequencing == time_min)
  #     if(nrow(temp_min) == 1){
  #       return(as.character(unlist(temp %>%
  #                                    filter(time_regimen_sequencing != time_min) %>%
  #                                    select(sample_ID))))
  #     }
  #     else{
  #       # if one sample has larger sample keep that one #
  #       plats_temps <- as.character(unique(temp_min$Sequence.Assay.ID))
  #       plat_keep <- plats_temps[which.max(as.numeric(unlist(regmatches(plats_temps, gregexpr("[[:digit:]]+", plats_temps)))))]
  #       # keep only samples with the larger platform #
  #       temp_min_plat <- temp_min %>%
  #         filter(Sequence.Assay.ID == plat_keep)
  #       if(nrow(temp_min_plat) == 1){
  #         return(as.character(unlist(temp %>%
  #                                      filter(sample_ID != temp_hist_min_plat$sample_ID) %>%
  #                                      select(sample_ID))))
  #       }
  #       else{
  #         # if no platform difference pick the one that was taken the latest (?) #
  #         keep <- as.character(temp_min_plat$sample_ID[which.max(as.numeric(unlist(lapply(regmatches(temp_min_plat$sample_ID, gregexpr("[[:digit:]]+", temp_min_plat$sample_ID)),
  #                                                                                         function(y){
  #                                                                                           paste0(y, collapse = "")
  #                                                                                         }))))])
  #         return(as.character(unlist(temp %>%
  #                                      filter(sample_ID != keep) %>%
  #                                      select(sample_ID))))
  #       }
  #     }
  #   }
  #
  # }))
  #
  # # rm duplicated samples #
  # samples_object_final <- samples_object %>%
  #   filter(!(sample_ID %in% rm))
  #
  # return(samples_object_final)
}
