# List of Drug Regimen Names by Cohort

A dataset containing the cancer-directed drug names and their synonyms.

## Usage

``` r
drug_regimen_list
```

## Format

A table for cancer-directed drug names associated with each data
release:

- cohort:

  GENIE BPC Project cancer. One of "NSCLC" (non-small cell lung cancer),
  "CRC" (colorectal cancer), "BrCa" (breast cancer), "PANC" (pancreatic
  cancer), "Prostate" (prostate cancer), and "BLADDER" (bladder cancer).

- cohort_data_release:

  GENIE BPC data release. Occasionally, drug names were updated across
  releases to include additional drug name synonyms.

- drug_name:

  Name of generic/ingredient cancer-directed drug

- drug_name_full:

  Name of generic/ingredient cancer-directed drug with associated
  synonyms in parentheses
