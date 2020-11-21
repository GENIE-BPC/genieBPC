#' create_cohort
#'
#' This function allows the user to create a relevant cohort for analysis based on histology, specific regimen,
#' regimen line, stage at diagnosis and centers to be considered.
#' This will return a dataset containing all patients matching the criterion of interests and their available survival data.
#' Specifically PFS_I, PFS_M and OS from the start of regimen.
#' @param treatment_dat dataframe containing the relevant information for therapies. eg: data_timeline_treatment.txt
#' @param center character vector containing the centers to be considered to create the cohort. eg: c("MSK", "DFCI"). By default
#' all centers found in the dataset inputted will be used.
#' @param regimen character vector containing the regimens to be considered to create the cohort.
#' eg: c("Carboplatin, Pemetrexed Disodium","Cisplatin, Pemetrexed Disodium"). By default
#' all regimens found in the dataset inputted will be used.
#' @param line numberic vector containing the line of therapies to be used to create the cohort. eg: c(1,2), which would keep
#' regimens specified in the argument above at the lines specified here. By default all lines found will be kept.
#' @param stage character vector specifying which stage at initial diagnosis to be considered.
#' @export
#' @import
#' dplyr
#' dtplyr
#' tibble


create_cohort <- function(treatment_dat, center = NULL, regimen = NULL, line = NULL, stage = NULL){

  # check params #
  if(is.null(center))
    center <- c("DFCI","MSK","VICC","UHN")
  if(is.null(regimen)){
    warning("We strongly recommend you focus on some regimens to study. All regimens found in treatment_dat will be used.")
    regimen <- unique(as.character(treatment_dat$REGIMEN))
  }
  if(is.null(line)){
    warning("We strongly recommend you focus on some regimen lines to study. All lines found in treatment_dat will be used.")
    line <- unique(as.numeric(as.character(treatment_dat$REGIMEN_NUMBER)))
  }

  treatment_dat %>%
    mutate(Center = case_when(
      grepl("DFCI", PATIENT_ID) ~ "DFCI",
      grepl("MSK", PATIENT_ID) ~ "MSK",
      grepl("VICC", PATIENT_ID) ~ "VICC",
      grepl("UHN", PATIENT_ID) ~ "UHN"
    )) %>%
    filter(REGIMEN %in% regimen,
           REGIMEN_NUMBER == line,
           Center %in% center) %>%
    mutate(os_time = OS_START_REGIMEN_DAYS/30.4375,
           pfs_I_time = PFS_I_START_REGIMEN_DAYS/30.4375,
           pfs_M_time = PFS_M_START_REGIMEN_DAYS/30.4375,
           ID = as.character(PATIENT_ID)) %>%
    rename(regimen = REGIMEN, line = REGIMEN_NUMBER, os_status = OS_START_REGIMEN_STATUS,
           pfs_I_status = PFS_I_START_REGIMEN_STATUS, pfs_M_status = PFS_M_START_REGIMEN_STATUS,
           dob = DOB_START_REGIMEN_DATE) %>%
    select(ID, Center, regimen, line, os_time, os_status,pfs_I_time,
           pfs_I_status,pfs_M_time, pfs_M_status,dob) %>%
    distinct()
}
