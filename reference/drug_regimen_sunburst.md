# Visualize drug regimen sequences in a sunburst plot

This function allows the user to visualize the complete treatment course
for selected cancer diagnoses.

## Usage

``` r
drug_regimen_sunburst(data_synapse, data_cohort, max_n_regimens = NULL, ...)
```

## Arguments

- data_synapse:

  The item from the nested list returned from \`pull_data_synapse()\`

- data_cohort:

  The list returned from the \`create_analytic_cohort()\` function call

- max_n_regimens:

  The maximum number of regimens displayed in the sunburst plot

- ...:

  Additional parameters passed to \`sunburstR::sunburst()\`

## Value

Returns data frame \`treatment_history\` and interactive plot
\`sunburst_plot\`

## Details

See the [drug_regimen_sunburst
vignette](https://genie-bpc.github.io/genieBPC/articles/drug_regimen_sunburst_vignette.html)
for additional details and examples.

## Examples

``` r
# Example 1 ----------------------------------
# Example using package test data
# get clinico-genomic files for a specific cohort
nsclc_sub <- create_analytic_cohort(
  data_synapse = genieBPC::nsclc_test_data,
  stage_dx = c("Stage III", "Stage IV")
)

# create sunburst plot
ex1 <- drug_regimen_sunburst(
  data_synapse = nsclc_test_data,
  data_cohort = nsclc_sub,
  max_n_regimens = 3
)

# Example 2 ----------------------------------
# using pull_data_synapse
nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
#> Error in .get_synapse_token(pat = pat): No credentials found. Have you authenticated yourself? Use
#> `set_synapse_credentials()` to set credentials for this session, or pass a
#> `pat` (see README:'Data Access & Authentication' for details on
#> authentication).

nsclc_stg_iv <- create_analytic_cohort(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  stage = "Stage IV"
)
#> Error: object 'nsclc_2_0' not found

ex2 <- drug_regimen_sunburst(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  data_cohort = nsclc_stg_iv,
  max_n_regimens = 3
)
#> Error: object 'nsclc_2_0' not found
```
