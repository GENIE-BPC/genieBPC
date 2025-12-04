# Tutorial: drug_regimen_sunburst

## Introduction

The
[`drug_regimen_sunburst()`](https://genie-bpc.github.io/genieBPC/reference/drug_regimen_sunburst.md)
function allows the user to visualize the complete treatment course for
selected diagnoses in a given analytic cohort. The function returns a
list containing a ‘treatment_history’ data frame and the interactive
plot ‘sunburst_plot’.

## Modifying Function Arguments

| \`drug_regimen_sunburst()\` Function Arguments |                                                                                                                 |
|------------------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| Argument                                       | Description                                                                                                     |
| `data_synapse`                                 | The item from the nested list returned from pull_data_synapse() corresponding to the cancer cohort of interest. |
| `data_cohort`                                  | The list returned from the \`create_analytic_cohort()\` function call.                                          |
| `max_n_regimens`                               | The number of lines of treatment displayed in the sunburst plot.                                                |

Note that the innermost circle of the sunburst plot indicates the first
therapies that patients received following diagnosis. The circle is
divided proportionally based on the percentage of patients that received
a particular treatment. Each subsequent ring of the sunburst corresponds
to a subsequent treatment regimen, such that the inner rings represent
earlier treatments. Together, the rings illustrate the distribution and
patterns of various treatment regimens.

## Setup

Before going through the tutorial, load the {genieBPC} library and log
into Synapse using the
[`set_synapse_credentials()`](https://genie-bpc.github.io/genieBPC/reference/set_synapse_credentials.md)
function. For more information on
[`set_synapse_credentials()`](https://genie-bpc.github.io/genieBPC/reference/set_synapse_credentials.md),
refer to the `Tutorial: pull_data_synapse()` vignette.

``` r
library(genieBPC)

set_synapse_credentials()
```

This tutorial will utilize the data downloaded in the
`Tutorial: pull_data_synapse()` vignette, as shown below:

``` r
nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
```

## Example

The example below creates a sunburst plot of the first three
cancer-directed regimens received by patients diagnosed with Stage IV
NSCLC.

``` r
nsclc_stg_iv <- create_analytic_cohort(data_synapse = nsclc_2_0$NSCLC_v2.0,
                                      stage = c("Stage IV"))

nsclc_treatment <- drug_regimen_sunburst(data_synapse = nsclc_2_0$NSCLC_v2.0,
                      data_cohort = nsclc_stg_iv,
                      max_n_regimens = 3)
```

``` r
head(nsclc_treatment$treatment_history)
#> # A tibble: 6 × 2
#>   path                                                                       Pop
#>   <chr>                                                                    <int>
#> 1 Afatinib Dimaleate                                                           1
#> 2 Afatinib Dimaleate-Afatinib Dimaleate, Carboplatin, Pemetrexed Disodium…     1
#> 3 Afatinib Dimaleate-Cisplatin, Pemetrexed Disodium-Osimertinib                1
#> 4 Afatinib Dimaleate-Erlotinib Hydrochloride-Bevacizumab, Carboplatin, Pe…     1
#> 5 Afatinib Dimaleate-Erlotinib Hydrochloride-Investigational Drug              1
#> 6 Afatinib Dimaleate-Gefitinib-Bevacizumab, Carboplatin, Pemetrexed Disod…     1
```

``` r
nsclc_treatment$sunburst_plot
```

Legend
