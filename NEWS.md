# genieBPC (development version)

* Clarify error messaging in `select_unique_ngs()` (#57)

* Clarify the `select_unique_ngs()` input parameters in the function 
documentation and vignette

* Update selecting functions to account for `tidyselect` v1.2.0 release 
(issue #83)

* Bioinformatics updates addressing reviewer comments (issue #75)

# genieBPC 1.0.1

* Add CRAN badge to README (issue #69)

* Update `drug_regimen_sunburst()` to pass additional arguments to 
`sunburstR::sunburst()` (issue #71)

* Added CRAN installation instructions to README (issue #72)

* Update handling of missing data when reading in csv files in `pull_data_synapse()` 
so that missing data is stored as NA and not as "" (issue #73).

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
