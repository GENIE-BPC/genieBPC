# Select cohort of patients for analysis

This function allows the user to create a cohort from the GENIE BPC data
based on cancer diagnosis information such as cancer cohort, treating
institution, histology, and stage at diagnosis, as well as
cancer-directed regimen information including regimen name and regimen
order. This function returns each of the clinical and genomic data files
subset on the patients that met criteria for the analytic cohort.
Documentation regarding the structure and contents of each file can be
found in the Analytic Data Guide corresponding to each data release, as
well as in the [Clinical Data Structure
vignette](https://genie-bpc.github.io/genieBPC/articles/clinical_data_structure_vignette.html).

## Usage

``` r
create_analytic_cohort(
  data_synapse,
  index_ca_seq = 1,
  institution,
  stage_dx,
  histology,
  regimen_drugs,
  regimen_type = "Exact",
  regimen_order,
  regimen_order_type,
  return_summary = FALSE
)
```

## Arguments

- data_synapse:

  The item from the nested list returned from pull_data_synapse() that
  corresponds to the cancer cohort of interest.

- index_ca_seq:

  Index cancer sequence. Default is 1, indicating the patient's first
  index cancer. The index cancer is also referred to as the BPC Project
  cancer in the GENIE BPC Analytic Data Guide; this is the cancer that
  met the eligibility criteria for the project and was selected at
  random for PRISSMM phenomic data curation. Specifying multiple index
  cancer sequences, e.g. index_ca_seq = c(1, 2) will return index
  cancers to patients with 1 index cancer and will return the first AND
  second index cancers to patients with multiple.

- institution:

  GENIE BPC participating institution. See lookup table
  \`cohort_institution\` for a list of institutions participating in
  curation of each cancer cohort. Default selection is all institutions.
  This parameter corresponds to the variable \`institution\` in the
  Analytic Data Guide.

- stage_dx:

  Stage at diagnosis. Must be one of "Stage I", "Stage II", "Stage III",
  "Stage I-III NOS", "Stage IV". The default selection is all stages.
  Note that if this parameter is specified, any cases that are missing
  stage information are automatically excluded from the resulting
  cohort. This parameter corresponds to the variable \`stage_dx\` in the
  Analytic Data Guide.

- histology:

  Cancer histology. For all cancer cohorts except for BrCa (breast
  cancer), this parameter corresponds to the variable
  \`ca_hist_adeno_squamous\` and must be one of "Adenocarcinoma",
  "Squamous cell", "Sarcoma", "Small cell carcinoma", "Carcinoma",
  "Other histologies/mixed tumor". For BrCa, this parameter corresponds
  to the variable \`ca_hist_brca\` and must be one of "Invasive lobular
  carcinoma", "Invasive ductal carcinoma", "Other histology". The
  default selection is all histologies. Note that if this parameter is
  specified, any cases that are missing histology information are
  automatically excluded from the resulting cohort.

- regimen_drugs:

  Vector with names of drugs in cancer-directed regimen, separated by a
  comma. For example, to specify a regimen consisting of Carboplatin and
  Pemetrexed, specify regimen_drugs = "Carboplatin, Pemetrexed".
  Acceptable values are found in the \`drug_regimen_list\` dataset
  provided with this package. This parameter corresponds to the variable
  \`regimen_drugs\` in the Analytic Data Guide.

- regimen_type:

  Indicates whether the regimen(s) specified in \`regimen_drugs\`
  indicates the exact regimen to return, or if regimens containing the
  drugs listed in \`regimen_drugs\` should be returned. Must be one of
  "Exact" or "Containing". The default is "Exact".

- regimen_order:

  Order of cancer-directed regimen. If multiple drugs are specified,
  \`regimen_order\` indicates the regimen order for all drugs; different
  values of \`regimen_order\` cannot be specified for different drug
  regimens. If multiple values are specified, e.g. c(1, 2), then drug
  regimens that met either order criteria are returned.

- regimen_order_type:

  Specifies whether the \`regimen_order\` parameter refers to the order
  of receipt of the drug regimen within the cancer diagnosis (across all
  other drug regimens; "within cancer") or the order of receipt of the
  drug regimen within the times that that drug regimen was administered
  (e.g. the first time carboplatin pemetrexed was received, out of all
  times that the patient received carboplatin pemetrexed; "within
  regimen"). Acceptable values are "within cancer" and "within regimen".

- return_summary:

  Specifies whether a summary table for the cohort is returned. Default
  is FALSE. The \`gtsummary\` package is required to return a summary
  table.

## Value

A list of data frames containing clinical and next generation sequencing
information for patients that met the specified criteria. Optionally, if
return_summary = TRUE, the list also includes summary tables for the
number of records per dataset (\`tbl_overall_summary\`) as well as
tables of key cancer diagnosis (\`tbl_cohort\`), cancer-directed regimen
(\`tbl_drugs\`) and next generation sequencing (\`tbl_ngs\`) variables.

## Details

See the [create_analytic_cohort
vignette](https://genie-bpc.github.io/genieBPC/articles/create_analytic_cohort_vignette.html)
for further documentation and examples.

## Author

Jessica Lavery

## Examples

``` r
# Examples using package test data
# Example 1 ----------------------------------
# Create a cohort of all patients with stage IV NSCLC adenocarcinoma and
# obtain all of their corresponding clinical and genomic data

ex1 <- create_analytic_cohort(
  data_synapse = genieBPC::nsclc_test_data,
  stage_dx = "Stage IV",
  histology = "Adenocarcinoma"
)

names(ex1)
#>  [1] "cohort_pt_char"            "cohort_ca_dx"             
#>  [3] "cohort_ca_dx_non_index"    "cohort_ca_drugs"          
#>  [5] "cohort_prissmm_imaging"    "cohort_prissmm_pathology" 
#>  [7] "cohort_prissmm_md"         "cohort_ngs"               
#>  [9] "cohort_mutations_extended" "cohort_fusions"           
#> [11] "cohort_cna"               

# Example 2 ----------------------------------
# Create a cohort of all NSCLC patients who received Cisplatin,
# Pemetrexed Disodium or Cisplatin, Etoposide as their first drug regimen
# for their first index NSCLC

ex2 <- create_analytic_cohort(
  data_synapse = genieBPC::nsclc_test_data,
  regimen_drugs = c(
    "Cisplatin, Pemetrexed Disodium",
    "Cisplatin, Etoposide"
  ),
  regimen_order = 1,
  regimen_order_type = "within cancer"
)

# Example 3 ----------------------------------
# Create a cohort of all NSCLC patients who received Cisplatin, Pemetrexed
# Disodium at any time throughout the course of treatment for their
# cancer diagnosis,
# but in the event that the patient received the drug multiple times,
# only select the first time.

ex3 <- create_analytic_cohort(
  data_synapse = genieBPC::nsclc_test_data,
  regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
  regimen_order = 1,
  regimen_order_type = "within regimen"
)

# Example 4 ----------------------------------
# Using create_analytic_cohort with pull_data_synapse
set_synapse_credentials()
#> Error in curl::curl_fetch_memory(url, handle = handle): Timeout was reached [auth-prod.prod.sagebase.org]:
#> Operation too slow. Less than 1 bytes/sec transferred the last 600 seconds

nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#> Error in .get_synapse_token(pat = pat): No credentials found. Have you authenticated yourself? Use
#> `set_synapse_credentials()` to set credentials for this session, or pass a
#> `pat` (see README:'Data Access & Authentication' for details on
#> authentication).

ex4 <- create_analytic_cohort(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  regimen_drugs = c("Cisplatin, Pemetrexed Disodium"),
  regimen_order = 1,
  regimen_order_type = "within regimen"
)
#> Error: object 'nsclc_2_0' not found
```
