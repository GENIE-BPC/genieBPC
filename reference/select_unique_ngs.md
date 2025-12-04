# Selecting corresponding unique next generation sequencing reports

For patients with multiple associated next generation (NGS) sequencing
reports, select one unique NGS report per patient for the purpose of
creating an analytic dataset based on user-defined criterion, including
OncoTree code, primary vs. metastatic tumor sample, and earliest vs.
most recent sample. If multiple reports for a patient remain available
after the user-defined specifications, or if no specifications are
provided, the panel with the largest number of genes is selected by
default. Sample optimization is performed in the order that the
arguments are specified in the function, regardless of the arguments’
order provided by the user. Namely, the OncoTree code is prioritized
first, sample type is prioritized second and finally the time is
prioritized last. For patients with exactly one genomic sample, that
unique genomic sample will be returned regardless of whether it meets
the user-specified parameters. Running the select_unique_ngs() function
will ensure that the resulting dataset returned by merging the next
generation sequencing report data onto the cohort_ca_dx dataset returned
by create_analytic_cohort() will maintain the structure of cohort_ca_dx
(either one record per patient or one record per diagnosis). Currently,
if multiple diagnoses per patient are returned from
create_analytic_cohort(), using select_unique_ngs() will select a single
NGS report per patient. In future iterations, this will be updated so
that one NGS report per diagnosis can be selected.

## Usage

``` r
select_unique_ngs(
  data_cohort,
  oncotree_code = NULL,
  sample_type = NULL,
  min_max_time = NULL
)
```

## Arguments

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

returns the 'cohort_ngs' object of the create_analytic_cohort with
unique genomic samples taken from each patients.

## Details

Note that the NGS dataset serves as the link between the clinical and
genomic data, where the NGS dataset includes one record per NGS report
per patient, including the NGS sample ID that is used to link to the
genomic data files. Merging data from the NGS report onto the analytic
cohort returned from create_analytic_cohort() therefore allows users to
utilize all clinical and genomic data available.

See the [select_unique_ngs
vignette](https://genie-bpc.github.io/genieBPC/articles/select_unique_ngs_vignette.html)
for further documentation and examples.

## Author

Karissa Whiting

## Examples

``` r
# Example 1 ----------------------------------
# Create a cohort of all patients with stage IV NSCLC of
# histology adenocarcinoma
set_synapse_credentials()
#> ✔ You are now connected to 'Synapse' with your Personal Access Token for this R session!

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

# select unique next generation sequencing reports for those patients
samples_data1 <- select_unique_ngs(
  data_cohort = ex1$cohort_ngs,
  sample_type = "Primary"
)
#> ✔ 63 patients with > 1 next generation sequencing reports were identified
#> ✔ 34 of 63 had a unique NGS report selected based on given criteria.
#> ! 29 of 63 did not have a unique NGS report selected based on the selected criteria or by having the largest panel, so a NGS report was selected at random (be sure to set a seed for reproducbility!) See `attributes(<your-results>)$random_samples` to view these sample IDs.

# Example 2 ----------------------------------
# Create a cohort of all NSCLC patients who
# received Cisplatin, Pemetrexed Disodium or Cisplatin,
# Etoposide as their first drug regimen
ex2 <- create_analytic_cohort(
  data_synapse = nsclc_2_0$NSCLC_v2.0,
  regimen_drugs = c(
    "Cisplatin, Pemetrexed Disodium",
    "Cisplatin, Etoposide"
  ),
  regimen_order = 1,
  regimen_order_type = "within regimen"
)

samples_data2 <- select_unique_ngs(
  data_cohort = ex2$cohort_ngs,
  oncotree_code = "NSCLCPD",
  sample_type = "Metastasis",
  min_max_time = "max"
)
#> ✔ 14 patients with > 1 next generation sequencing reports were identified
```
