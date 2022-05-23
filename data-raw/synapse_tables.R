synapse_tables <- readxl::read_excel("data-raw/synapse_tables.xlsx")

usethis::use_data(synapse_tables, internal = FALSE, overwrite = TRUE)
