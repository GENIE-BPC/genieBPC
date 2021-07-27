synapse_tables <- tidyr::tibble(
  cohort = c(rep("NSCLC", 22), rep("CRC", 12)),
  df = c(
          # NSCLC
         rep(c("pt_char", "ca_dx_index", "ca_dx_non_index","ca_drugs",
         "prissmm_pathology", "prissmm_imaging","prissmm_md", "cpt",
         "cna", "fusions","mutations_extended"),2),
          # CRC
         "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
         "prissmm_pathology", "prissmm_imaging", "prissmm_md",
         "tm", "cpt","cna", "fusions","mutations_extended"),
  version = c(rep("v1.1", 11),rep("v2.1", 11),rep("v1.1", 12)),
  synapse_id = c(# NSCLC 1.1
                 "syn22418979", "syn22418974", "syn22418975", "syn22418980", "syn22418982",
                 "syn22418981", "syn22418986", "syn22418987", "syn22334132", "syn22334134",
                 "syn22334131",
                 # NSCLC 2.1
                 "syn25985884","syn25985882","syn25985883",
                 "syn25985885","syn25985887","syn25985886",
                 "syn25985888","syn25985889",
                 "syn25471850","syn25471854","syn25471844",

                 # CRC
                 "syn24168397", "syn24168395", "syn24168396", "syn24168398", "syn24168400",
                 "syn24168399", "syn24168401", "syn24168403", "syn24168402", "syn23593204",
                 "syn23593208", "syn23593182"
  )
)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
