synapse_tables <- tidyr::tibble(
  cohort = c(rep("NSCLC", 8), rep("CRC", 9)),
  df = c("pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs", "prissmm_pathology", "prissmm_imaging", "prissmm_md", "cpt",
         "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs", "prissmm_pathology", "prissmm_imaging", "prissmm_md", "tm", "cpt"),
  version = c(rep("v1.1", 17)),
  synapse_id = c("syn22418979", "syn22418974", "syn22418975", "syn22418980", "syn22418982", "syn22418981", "syn22418986", "syn22418987",
                 "syn24168397", "syn24168395", "syn24168396", "syn24168398", "syn24168400", "syn24168399", "syn24168401", "syn24168403", "syn24168402"
  )
)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
