# genieBPC (development version)

* Removed `TramineR` from dependencies file (issue # 50)

* Pull the most recent public and consortium data (issue #42)

* Removed `synapser` from dependencies file (issue #45)

* Fixed `cpt_sample_type`/`sample_type` (issue #43)

* Added `.is_connected_to_genie()` to replace `.check_synapse_login()` and 
return logical TRUE/FALSE. This is an internal function that can be used to 
trigger tests/checks as needed and will hopefully allow the package to now 
pass the CRAN checks.

* Added drug names for breast cancer cohort

* Update examples to use test data, when possible

# genieBPC 0.1.0

* Added a `NEWS.md` file to track changes to the package.
