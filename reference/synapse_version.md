# Return list of available GENIE BPC data releases

GENIE BPC data are updated periodically to add variables and reflect
additional data cleaning. Each time the data are updated the data
release version number is incremented. The \`synapse_version()\`
function will get available version numbers for each cohort to help the
user determine what is the most recent version for each cohort.

## Usage

``` r
synapse_version(cohort = NULL, most_recent = FALSE)
```

## Arguments

- cohort:

  Vector specifying the cohort(s) of interest. Cohorts must be one of
  "NSCLC" (Non-Small Cell Lung Cancer), "CRC" (Colorectal Cancer), or
  "BrCa" (Breast Cancer), "PANC" (Pancreatic Cancer), "Prostate"
  (Prostate Cancer), and "BLADDER" (Bladder Cancer).

- most_recent:

  Indicates whether the function will return only the most recent
  version number for each cohort (\`most_recent\` = TRUE) or all
  available version numbers for each cohort (\`most_recent\`= FALSE)

## Value

Returns a table containing the available versions for each cohort.
Consortium releases are restricted to GENIE BPC consortium members.

## Details

Specifies the version numbers available for each cancer cohort. Version
numbers are specified as part of the call to \`pull_data_synapse()\`.

## Examples

``` r
synapse_version()
#> # A tibble: 14 × 4
#>    cohort   version         release_date versions_returned
#>    <chr>    <chr>           <chr>        <chr>            
#>  1 BLADDER  v1.1-consortium 2022-11      All Versions     
#>  2 BLADDER  v1.2-consortium 2023-11      All Versions     
#>  3 BrCa     v1.1-consortium 2021-10      All Versions     
#>  4 BrCa     v1.2-consortium 2022-10      All Versions     
#>  5 CRC      v2.0-public     2022-10      All Versions     
#>  6 CRC      v1.3-consortium 2024-02      All Versions     
#>  7 NSCLC    v1.1-consortium 2020-10      All Versions     
#>  8 NSCLC    v2.0-public     2022-05      All Versions     
#>  9 NSCLC    v2.2-consortium 2024-02      All Versions     
#> 10 NSCLC    v3.1-consortium 2024-04      All Versions     
#> 11 PANC     v1.1-consortium 2022-02      All Versions     
#> 12 PANC     v1.2-consortium 2023-01      All Versions     
#> 13 Prostate v1.1-consortium 2022-03      All Versions     
#> 14 Prostate v1.2-consortium 2023-01      All Versions     
synapse_version(most_recent = TRUE)
#> # A tibble: 6 × 4
#>   cohort   version         release_date versions_returned   
#>   <chr>    <chr>           <chr>        <chr>               
#> 1 BLADDER  v1.2-consortium 2023-11      Most Recent Versions
#> 2 BrCa     v1.2-consortium 2022-10      Most Recent Versions
#> 3 CRC      v1.3-consortium 2024-02      Most Recent Versions
#> 4 NSCLC    v3.1-consortium 2024-04      Most Recent Versions
#> 5 PANC     v1.2-consortium 2023-01      Most Recent Versions
#> 6 Prostate v1.2-consortium 2023-01      Most Recent Versions
```
