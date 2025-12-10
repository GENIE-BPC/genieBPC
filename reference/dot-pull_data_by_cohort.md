# Function to retrieve data by synapse ID

Function to retrieve data by synapse ID

## Usage

``` r
.pull_data_by_cohort(version_num_df, token, download_location)
```

## Arguments

- version_num_df:

  a dataframe of 'Synapse' IDs

- token:

  a 'Synapse' token

- download_location:

  if \`NULL\` (default), data will be returned as a list of dataframes
  with requested data as list items. Otherwise, specify a folder path to
  have data automatically downloaded there.

## Value

downloaded 'Synapse' data as a list if \`download_location\`= \`NULL, or
to a local path

## Examples

``` r
temp_directory <- tempdir()

syn_df <- data.frame(
  cohort = c("NSCLC", "NSCLC"),
  version = c("v2.2-consortium", "v2.0-public"),
  version_num = c("NSCLC_v2.2", "NSCLC_v2.0"),
  download_folder = c(temp_directory, temp_directory),
  df = c("pt_char", "ca_dx_index"),
  synapse_id = c("syn53470868", "syn30350575")
)

.pull_data_by_cohort(
  version_num_df = syn_df,
  token = .get_synapse_token(), download_location = NULL
)
#> Error in dplyr::mutate(., file_info = map(.data$query_url, function(x) {    requestedObjects <- list(includeEntity = TRUE, includeAnnotations = TRUE,         includeFileHandles = TRUE, includeRestrictionInformation = TRUE)    res_per_id <- httr::POST(url = x, body = jsonlite::toJSON(requestedObjects,         pretty = TRUE, auto_unbox = TRUE), httr::add_headers(Authorization = paste("Bearer ",         token, sep = "")), httr::content_type("application/json"))    entityBundle <- httr::content(res_per_id, "parsed", encoding = "UTF-8")    switch(entityBundle$restrictionInformation$hasUnmetAccessRequirement,         cli::cli_abort("Your 'Synapse' account has unmet access requirements.\n                          Have you accepted the 'Terms of Use' for this dataset?\n                          See 'Synapse' portal (`https://www.synapse.org/`) for more info."))    file_info <- entityBundle$fileHandles[[1]]    bind_cols(type = file_info$contentType, name = file_info$fileName,         file_handle_id = file_info$id)})): ℹ In argument: `file_info = map(...)`.
#> Caused by error in `map()`:
#> ℹ In index: 1.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Timeout was reached [auth-prod.prod.sagebase.org]:
#> Operation too slow. Less than 1 bytes/sec transferred the last 600 seconds

#
```
