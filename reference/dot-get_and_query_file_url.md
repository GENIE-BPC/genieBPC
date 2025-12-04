# Get URL for a given 'Synapse' file and download to local machine

Get URL for a given 'Synapse' file and download to local machine

## Usage

``` r
.get_and_query_file_url(
  version_num,
  file_handle_id,
  synapse_id,
  df,
  name,
  download_folder,
  download_location,
  token,
  file_endpoint_url
)
```

## Arguments

- version_num:

  cohort name and version

- file_handle_id:

  'Synapse' file handle ID

- synapse_id:

  'Synapse' ID

- df:

  package designated name of file

- name:

  file name from 'Synapse'

- download_folder:

  location to download data

- token:

  Synapse token

- file_endpoint_url:

  'Synapse' endpoint for file info

## Value

list of 'Synapse' data frames

## Examples

``` r
if (FALSE) {

file <- data.frame(
  version_num = "NSCLC_v2.1",
  file_handle_id = c("79432768"),
  synapse_id = c("syn25985884"),
  df = c("pt_char"),
  name = c("patient_level_dataset.csv"),
  download_folder = file.path(tempdir(), "NSCLC_v2.1")
)

purrr::pmap(file, .get_and_query_file_url)
}
```
