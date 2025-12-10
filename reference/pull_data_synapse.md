# Obtain clinical & genomic data files for GENIE BPC Project

Function to access specified versions of clinical and genomic GENIE BPC
data from
[Synapse](https://www.synapse.org/#!Synapse:syn21226493/wiki/599164) and
read them into the R environment. See the [pull_data_synapse
vignette](https://genie-bpc.github.io/genieBPC/articles/pull_data_synapse_vignette.html)
for further documentation and examples.

## Usage

``` r
pull_data_synapse(
  cohort = NULL,
  version = NULL,
  download_location = NULL,
  pat = NULL
)
```

## Arguments

- cohort:

  Vector or list specifying the cohort(s) of interest. Must be one of
  "NSCLC" (Non-Small Cell Lung Cancer), "CRC" (Colorectal Cancer), or
  "BrCa" (Breast Cancer), "PANC" (Pancreatic Cancer), "Prostate"
  (Prostate Cancer), "BLADDER" (Bladder Cancer), and "RENAL" (Renal
  Cancer). This is not case sensitive.

- version:

  Vector specifying the version of the cohort. Must match one of the
  release versions available for the specified \`cohort\` (see
  \`synapse_version()\` for available cohort versions). When entering
  multiple cohorts, it is inferred that the order of the version numbers
  passed corresponds to the order of the cohorts passed. Therefore,
  \`cohort\` and \`version\` must be in the same order to ensure the
  correct data versions are pulled. See examples below for details.

- download_location:

  if \`NULL\` (default), data will be returned as a list of dataframes
  with requested data as list items. Otherwise, specify a folder path to
  have data automatically downloaded there. When a path is specified,
  data are not read into the R environment.

- pat:

  'Synapse' personal access token

## Value

Returns a nested list of clinical and genomic data corresponding to the
specified cohort(s).

## Authentication

To access data, users must have a valid 'Synapse' account with
permission to access the data set and they must have accepted any
necessary 'Terms of Use'. Users must always authenticate themselves in
their current R session. (see [README: Data Access and
Authentication](https://genie-bpc.github.io/genieBPC/articles/pull_data_synapse_vignette.html)

for details). To set your 'Synapse' Personal Access Token (PAT) during
each session, call: \`set_synapse_credentials(pat = "your_pat")\`.

If your credentials are stored as an environmental variable, you do not
need to call \`set_synapse_credentials()\` explicitly each session. To
store authentication information in your environmental variables, add
the following to your .Renviron file, then restart your R session '
(tip: you can use \`usethis::edit_r_environ()\` to easily open/edit this
file):

- \`SYNAPSE_PAT = \<your-pat\>\`

Alternatively, you can pass your PAT to each individual data pull
function if preferred, although it is recommended that you manage your
PATs outside of your scripts for security purposes.

## Analytic Data Guides

Documentation corresponding to the clinical data files can be found on
'Synapse' in the Analytic Data Guides:

- [NSCLC v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn23002641)

- [NSCLC v2.2-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn53463493)

- [NSCLC v2.0-Public Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn30557304)

- [NSCLC v3.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn58597690)

- [CRC v1.3-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn53463650)

- [CRC v2.0-Public Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn31751466)

- [CRC v3.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn64425776)

- [BrCa v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn26077313)

- [BrCa v1.2-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn32330194)

- [BrCa v1.0-Public Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn71825209)

- [BLADDER v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn30787692)

- [BLADDER v1.2-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn53018714)

- [PANC v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn29787285)

- [PANC v1.2-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn50612821)

- [Prostate v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn30148714)

- [Prostate v1.2-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn50612204)

- [RENAL v1.1-Consortium Analytic Data
  Guide](https://www.synapse.org/#!Synapse:syn69750457)

## Author

Karissa Whiting, Michael Curry

## Examples

``` r
# Example 1 ----------------------------------
# Set up 'Synapse' credentials
set_synapse_credentials()
#> ✔ You are now connected to 'Synapse' with your Personal Access Token for this R session!

# Print available versions of the data
synapse_version(most_recent = TRUE)
#> # A tibble: 7 × 4
#>   cohort   version         release_date versions_returned   
#>   <chr>    <chr>           <chr>        <chr>               
#> 1 BLADDER  v1.2-consortium 2023-11      Most Recent Versions
#> 2 BrCa     v1.0-public     2025-12      Most Recent Versions
#> 3 CRC      v3.1-consortium 2025-03      Most Recent Versions
#> 4 NSCLC    v3.1-consortium 2024-04      Most Recent Versions
#> 5 PANC     v1.2-consortium 2023-01      Most Recent Versions
#> 6 Prostate v1.2-consortium 2023-01      Most Recent Versions
#> 7 RENAL    v1.1-consortium 2025-10      Most Recent Versions

# Pull version 2.0-public for non-small cell lung cancer
# and version 2.0-public for colorectal cancer data

 ex1 <- pull_data_synapse(
   cohort = c("NSCLC", "CRC"),
   version = c("v2.0-public", "v2.0-public")
 )
#> ✔ pt_char has been imported for "CRC_v2.0"
#> ✔ ca_dx_index has been imported for "CRC_v2.0"
#> ✔ ca_dx_non_index has been imported for "CRC_v2.0"
#> ✔ ca_drugs has been imported for "CRC_v2.0"
#> ✔ prissmm_imaging has been imported for "CRC_v2.0"
#> ✔ prissmm_pathology has been imported for "CRC_v2.0"
#> ✔ prissmm_md has been imported for "CRC_v2.0"
#> ✔ tumor_marker has been imported for "CRC_v2.0"
#> ✔ cpt has been imported for "CRC_v2.0"
#> ✔ mutations_extended has been imported for "CRC_v2.0"
#> ✔ fusions has been imported for "CRC_v2.0"
#> ✔ cna has been imported for "CRC_v2.0"
#> ✔ pt_char has been imported for "NSCLC_v2.0"
#> ✔ ca_dx_index has been imported for "NSCLC_v2.0"
#> ✔ ca_dx_non_index has been imported for "NSCLC_v2.0"
#> ✔ ca_drugs has been imported for "NSCLC_v2.0"
#> ✔ prissmm_imaging has been imported for "NSCLC_v2.0"
#> ✔ prissmm_pathology has been imported for "NSCLC_v2.0"
#> ✔ prissmm_md has been imported for "NSCLC_v2.0"
#> ✔ cpt has been imported for "NSCLC_v2.0"
#> ✔ mutations_extended has been imported for "NSCLC_v2.0"
#> ✔ fusions has been imported for "NSCLC_v2.0"
#> ✔ cna has been imported for "NSCLC_v2.0"

 names(ex1)
#> [1] "CRC_v2.0"   "NSCLC_v2.0"
```
