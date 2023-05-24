# genieBPC (development version)

# genieBPC (1.1.0.9000)

* Updated `genieBPC::nsclc_test_data` to randomly sample from cancer diagnosis dataset, stratified by stage, and incorporate genomic data into dataset.

# genieBPC 1.1.0

* Add PANC v1.1-consortium, Prostate v1.1-consortium, BrCa 1.2-consortium, PANC
v1.2-consortium, Prostate v1.2-consortium, CRC 2.0-public data releases

* Removed `fetch_samples()` function (#91)

* Updated `pull_data_synapse()` to read "NA" as NA values instead of as character "NA" so that CNA columns are numeric, as expected (#88)

* Ensure consistent order of datasets returned from calls to
`pull_data_synapse()`

* Added citations & links to GENIE BPC publications to README

# genieBPC 1.0.1

* Updates to `select_unique_ngs()` documentation and vignettes, as well as
clarification of the messaging returned when the function is run (#57)

* Update selecting functions to account for `tidyselect` v1.2.0 release 
(issue #83)

* Documentation updates following Bioinformatics reviewer comments (issue #75)

* Update `drug_regimen_sunburst()` to pass additional arguments to
`sunburstR::sunburst()` (issue #71)

* Added CRAN installation instructions to README (issue #72)

* Add CRAN badge to README (issue #69)

* Update handling of missing data when reading in csv files in
`pull_data_synapse()` so that missing data is stored as NA and not as "" (issue
#73).

# genieBPC 1.0.0

* Changed `print()` to `message()` in `select_unique_ngs()`

* Removed `TramineR` from dependencies file (issue #50)

* Incorporated the most recent public and consortium data releases (issue #42)

* Removed `synapser` from dependencies file (issue #45)

* Fixed `cpt_sample_type`/`sample_type` issue in `fetch_samples.R` (issue #43)

* Added `.is_connected_to_genie()` to replace `.check_synapse_login()` and 
return logical TRUE/FALSE. This is an internal function that can be used to 
trigger tests/checks as needed and will hopefully allow the package to now 
pass the CRAN checks.

* Added drug names for breast cancer cohort

* Update examples to use test data, when possible

# genieBPC 0.1.0

* Added a `NEWS.md` file to track changes to the package.
