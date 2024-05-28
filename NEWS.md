# genieBPC (development)

* Update `drug_regimen_list` lookup table to include drug names by data release, as opposed to by cohort. It is possible that drug names were modified slightly across data releases (#132)

* NEW FEATURE: Users can now pass their Personal Access Token (as well as username/ password) to set up authentication for accessing all data. 

* BREAKING CHANGE: Users must use `set_synapse_credentials()` at the top of all scripts and explicitly set username/password or PAT for each session.


# genieBPC 1.1.1

* Updated code for default institution in `create_analytic_cohort()` to fix bug if the `cohort` = "Prostate" (#130)

* Add 'cohort' parameter to `synapse_version()` to enable the user to subset by cohort(s) of interest

* Updated `genieBPC::nsclc_test_data` to ensure unique sample ids for CNA file.

* Updated `genieBPC::nsclc_test_data` to randomly sample from cancer diagnosis dataset, stratified by stage, and incorporate genomic data into dataset.

* Updated README and 'Pull Data Synapse Vignette' to clarify instructions for registering for a 'Synapse' account (#100, #105)

* Add publications (#96, #110)

* Add information about genomic differences between genomic data downloaded from cBioPortal versus Synapse (#92)

* Update tests to test across all data releases (#56)

* Fix bug related to radiation therapy data not being returned in `create_analytic_cohort()` 

### Data Release Updates

* Added **BLADDER v1.2-consortium** release

* Added **NSCLC 2.3-consortium** release

* Added **CRC v1.3-consortium** release

* Removed **NSCLC 2.1-consortium** release - Due to the inadvertent inclusion of data indicative of ages over 89 for data associated with the Project GENIE BioPharma Collaborative, this release was replaced with the **NSCLC 2.3-consortium** release. 

* Removed **CRC v1.1-consortium** release - Due to the inadvertent inclusion of data indicative of ages over 89 for data associated with the Project GENIE BioPharma Collaborative, this release was replaced with the newly added **CRC v1.3-consortium** release. 

* Removed **CRC v1.2-consortium** release - Due to the inadvertent inclusion of data indicative of ages over 89 for data associated with the Project GENIE BioPharma Collaborative, this release was replaced with the newly added **CRC v1.3-consortium** release. 


# genieBPC 1.1.0

* Removed `fetch_samples()` function (#91)

* Updated `pull_data_synapse()` to read "NA" as NA values instead of as character "NA" so that CNA columns are numeric, as expected (#88)

* Ensure consistent order of datasets returned from calls to
`pull_data_synapse()`

* Added citations & links to GENIE BPC publications to README

### Data Release Updates

* Added **PANC v1.1-consortium** release

* Added **Prostate v1.1-consortium** release

* Added **BrCa 1.2-consortium** release

* Added **PANC v1.2-consortium** release

* Added **Prostate v1.2-consortium** release

* Added **CRC 2.0-public** release


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

* Removed `synapser` from dependencies file (issue #45)

* Fixed `cpt_sample_type`/`sample_type` issue in `fetch_samples.R` (issue #43)

* Added `.is_connected_to_genie()` to replace `.check_synapse_login()` and 
return logical TRUE/FALSE. This is an internal function that can be used to 
trigger tests/checks as needed and will hopefully allow the package to now 
pass the CRAN checks.

* Added drug names for breast cancer cohort

* Update examples to use test data, when possible

### Data Release Updates

* Incorporated the most recent public and consortium data releases (issue #42)


# genieBPC 0.1.0

* Added a `NEWS.md` file to track changes to the package.
