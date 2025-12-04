# Select unique NGS report when multiple are available

See \`select_unique_ngs\` for details on selection criteria

## Usage

``` r
.resolve_duplicates(x, data_cohort, oncotree_code, sample_type, min_max_time)
```

## Arguments

- x:

  sample ID to select unique NGS report for

- data_cohort:

  CPT (NGS) dataframe returned from the create_analytic_cohort function

- oncotree_code:

  character vector specifying which sample OncoTree codes to keep. See
  "cpt_oncotree_code" column of data_cohort argument above to get
  options.

- sample_type:

  character specifying which type of genomic sample to prioritize,
  options are "Primary", "Local" and "Metastasis". Default is to not
  select a NGS sample based on the sample type.

- min_max_time:

  character specifying if the first or last genomic sample recorded
  should be kept. Options are "min" (first) and "max" (last).

## Value

a dataframe of samples with one observation per patient.

## Examples

``` r
nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#> Error in .get_synapse_token(pat = pat): No credentials found. Have you authenticated yourself? Use
#> `set_synapse_credentials()` to set credentials for this session, or pass a
#> `pat` (see README:'Data Access & Authentication' for details on
#> authentication).

ex1 <- create_analytic_cohort(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  stage_dx = c("Stage IV"),
  histology = "Adenocarcinoma"
)
#> Error: object 'nsclc_2_0' not found

 samples_data1 <- .resolve_duplicates(
   x = "GENIE-MSK-P-0025741",
  data_cohort = ex1$cohort_ngs,
  oncotree_code = "LUAD",
  sample_type = "Metastasis",
  min_max_time = "max"
)
#> Error: object 'ex1' not found

samples_data2 <- .resolve_duplicates(
   x = "GENIE-MSK-P-0025741",
  data_cohort = ex1$cohort_ngs,
  oncotree_code = "LUAD",
  sample_type = "Primary",
  min_max_time = "max"
)
#> Error: object 'ex1' not found
```
