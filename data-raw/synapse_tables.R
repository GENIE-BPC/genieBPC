synapse_tables <- tidyr::tibble(
  cohort = c(rep("NSCLC", 12*3),
             rep("CRC", 13*2),
             rep("BrCa", 12)),
  df = c(
    # NSCLC
    rep(c(
      "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
      "prissmm_pathology", "prissmm_imaging", "prissmm_md", "cpt",
      "cna", "fusions", "mutations_extended", "variable_synopsis"), 3),
    # CRC
    rep(c("pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
    "prissmm_pathology", "prissmm_imaging", "prissmm_md",
    "tm", "cpt", "cna", "fusions", "mutations_extended", "variable_synopsis"), 2),
    # BrCa
    rep(c(
      "pt_char", "ca_dx_index", "ca_dx_non_index", "ca_drugs",
      "prissmm_pathology", "prissmm_imaging", "prissmm_md", "cpt",
      "cna", "fusions", "mutations_extended", "variable_synopsis"), 1)
  ),
  version = c(#NSCLC
              rep("v1.1-consortium", 12),
              rep("v2.1-consortium", 12),
              rep("v2.0-public", 12),
              # CRC
              rep("v1.1-consortium", 13),
              rep("v1.2-consortium", 13),
              # BrCa
              rep("v1.1-consortium", 12)),
  # order needs to match order that datasets are specified above
  synapse_id = c(
    # NSCLC 1.1-consortium
    "syn22418979", "syn22418974", "syn22418975", "syn22418980", "syn22418982",
    "syn22418981", "syn22418986", "syn22418987", "syn22334132", "syn22334134",
    "syn22334131", "syn22335627",
    # NSCLC 2.1-consortium
    "syn25985884", "syn25985882", "syn25985883", "syn25985885", "syn25985887",
    "syn25985886", "syn25985888", "syn25985889", "syn25471850", "syn25471854",
    "syn25471844", "syn26028547",
    # NSCLC 2.0-public
    "syn28554323", "syn28554321", "syn28554322", "syn28554324",
    "syn25985885", "syn28554325", "syn28554327", "syn28554328",
    "syn27200491", "syn27200494", "syn27200447",
    "syn25471844",
    # CRC 1.1-consortium
    "syn24168397", "syn24168395", "syn24168396", "syn24168398", "syn24168400",
    "syn24168399", "syn24168401", "syn24168403", "syn24168402", "syn23593204",
    "syn23593208", "syn23593182", "syn24167537",
    # CRC 1.2-consortium
    "syn26046793", "syn26046791", "syn26046792", "syn26046794", "syn26046796",
    "syn26046795", "syn26046797", "syn26046799", "syn26046798", "syn25999008",
    "syn25999010", "syn25999007", "syn26077307",
    # BrCa 1.1-consortium
    "syn26253365", "syn26253363", "syn26253364", "syn26253366",
    "syn26253368", "syn26253367", "syn26253369", "syn26253370",
    "syn24994182", "syn24994184", "syn24994179", "syn26077309"
  )
)

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
