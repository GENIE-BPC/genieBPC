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

ex1 <- create_analytic_cohort(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  stage_dx = c("Stage IV"),
  histology = "Adenocarcinoma"
)

 samples_data1 <- .resolve_duplicates(
   x = "GENIE-MSK-P-0025741",
  data_cohort = ex1$cohort_ngs,
  oncotree_code = "LUAD",
  sample_type = "Metastasis",
  min_max_time = "max"
)

samples_data2 <- .resolve_duplicates(
   x = "GENIE-MSK-P-0025741",
  data_cohort = ex1$cohort_ngs,
  oncotree_code = "LUAD",
  sample_type = "Primary",
  min_max_time = "max"
)
```
