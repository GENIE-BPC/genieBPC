synapse_tables <- tidyr::tibble(
  cohort = c(rep("NSCLC", 11), rep("CRC", 12)),
  df = c(
          # NSCLC
         "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
         "prissmm_pathology", "prissmm_imaging", "prissmm_md", "cpt","cna", "fusions","mutations_extended",
          # CRC
         "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
         "prissmm_pathology", "prissmm_imaging", "prissmm_md",
         "tm", "cpt","cna", "fusions","mutations_extended"),
  version = c(rep("v1.1", 23)),
  synapse_id = c(# NSCLC
                 "syn22418979", "syn22418974", "syn22418975", "syn22418980", "syn22418982",
                 "syn22418981", "syn22418986", "syn22418987", "syn22334132", "syn22334134",
                 "syn22334131",

                 # CRC
                 "syn24168397", "syn24168395", "syn24168396", "syn24168398", "syn24168400",
                 "syn24168399", "syn24168401", "syn24168403", "syn24168402", "syn23593204",
                 "syn23593208", "syn23593182"
  )
)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
