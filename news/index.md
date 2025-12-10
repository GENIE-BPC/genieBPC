# Changelog

## genieBPC 2.1.1

- Add CRC v3.1-consortium, RENAL v1.1-consortium, and BrCa v1.0-public
  data releases

- Update tests to use setup.R file and to only run current data releases
  on CRAN and GitHub Actions (all data releases are tested when running
  locally)

## genieBPC 2.1.0

- Update
  [`create_analytic_cohort()`](https://genie-bpc.github.io/genieBPC/reference/create_analytic_cohort.md)
  to allow cohorts to be created based on multiple cancer types.
  Previously, only one cancer type at a time was permitted.

- Update GitHub actions to v4

- Breaking change: Remove access to GENIE BPC data on Synapse via
  username and password per updated Synapse requirements. Access to the
  data is now via Personal Access Token (PAT) only.

- Deprecate functions allowing data access by username and password.

  - Remove `.get_token_by_username()` function
  - Remove `.get_token_by_pat()` function (it is now redundant with
    [`.get_synapse_token()`](https://genie-bpc.github.io/genieBPC/reference/dot-get_synapse_token.md))
  - added
    [`.verify_pat_works()`](https://genie-bpc.github.io/genieBPC/reference/dot-verify_pat_works.md)
    helper function

## genieBPC 2.0.1

CRAN release: 2024-07-11

- Update ‘cohort’ parameter of
  [`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
  and
  [`synapse_version()`](https://genie-bpc.github.io/genieBPC/reference/synapse_version.md)
  to not be case-sensitive
  ([\#120](https://github.com/GENIE-BPC/genieBPC/issues/120))

- `synapse_version(most_recent = TRUE)` now returns one row per cohort,
  as opposed to one row per cohort and type of data release (consortium
  vs public) ([\#128](https://github.com/GENIE-BPC/genieBPC/issues/128))

- Update `drug_regimen_list` lookup table to include drug names by data
  release, as opposed to by cohort. It is possible that drug names were
  modified slightly across data releases
  ([\#132](https://github.com/GENIE-BPC/genieBPC/issues/132))

- NEW FEATURE: Users can now pass their Personal Access Token (as well
  as username/ password) to set up authentication for accessing all
  data. ([\#119](https://github.com/GENIE-BPC/genieBPC/issues/119))

- BREAKING CHANGE: Users must use
  [`set_synapse_credentials()`](https://genie-bpc.github.io/genieBPC/reference/set_synapse_credentials.md)
  at the top of all scripts and explicitly set username/password or PAT
  for each session.

- Added **NSCLC v3.1-consortium** release
  ([\#137](https://github.com/GENIE-BPC/genieBPC/issues/137))

- Read in structural variant files for NSCLC v2.2-consortium, CRC
  v1.3-consortium, and Bladder v1.2-consortium. For more information on
  structural variant data, see:
  <https://docs.cbioportal.org/file-formats/#structural-variant-data>.

## genieBPC 1.1.1

CRAN release: 2024-03-29

- Updated code for default institution in
  [`create_analytic_cohort()`](https://genie-bpc.github.io/genieBPC/reference/create_analytic_cohort.md)
  to fix bug if the `cohort` = “Prostate”
  ([\#130](https://github.com/GENIE-BPC/genieBPC/issues/130))

- Add ‘cohort’ parameter to
  [`synapse_version()`](https://genie-bpc.github.io/genieBPC/reference/synapse_version.md)
  to enable the user to subset by cohort(s) of interest

- Updated
  [`genieBPC::nsclc_test_data`](https://genie-bpc.github.io/genieBPC/reference/nsclc_test_data.md)
  to ensure unique sample ids for CNA file.

- Updated
  [`genieBPC::nsclc_test_data`](https://genie-bpc.github.io/genieBPC/reference/nsclc_test_data.md)
  to randomly sample from cancer diagnosis dataset, stratified by stage,
  and incorporate genomic data into dataset.

- Updated README and ‘Pull Data Synapse Vignette’ to clarify
  instructions for registering for a ‘Synapse’ account
  ([\#100](https://github.com/GENIE-BPC/genieBPC/issues/100),
  [\#105](https://github.com/GENIE-BPC/genieBPC/issues/105))

- Add publications
  ([\#96](https://github.com/GENIE-BPC/genieBPC/issues/96),
  [\#110](https://github.com/GENIE-BPC/genieBPC/issues/110))

- Add information about genomic differences between genomic data
  downloaded from cBioPortal versus Synapse
  ([\#92](https://github.com/GENIE-BPC/genieBPC/issues/92))

- Update tests to test across all data releases
  ([\#56](https://github.com/GENIE-BPC/genieBPC/issues/56))

- Fix bug related to radiation therapy data not being returned in
  [`create_analytic_cohort()`](https://genie-bpc.github.io/genieBPC/reference/create_analytic_cohort.md)

#### Data Release Updates

- Added **BLADDER v1.2-consortium** release

- Added **NSCLC 2.3-consortium** release

- Added **CRC v1.3-consortium** release

- Removed **NSCLC 2.1-consortium** release - Due to the inadvertent
  inclusion of data indicative of ages over 89 for data associated with
  the Project GENIE BioPharma Collaborative, this release was replaced
  with the **NSCLC 2.3-consortium** release.

- Removed **CRC v1.1-consortium** release - Due to the inadvertent
  inclusion of data indicative of ages over 89 for data associated with
  the Project GENIE BioPharma Collaborative, this release was replaced
  with the newly added **CRC v1.3-consortium** release.

- Removed **CRC v1.2-consortium** release - Due to the inadvertent
  inclusion of data indicative of ages over 89 for data associated with
  the Project GENIE BioPharma Collaborative, this release was replaced
  with the newly added **CRC v1.3-consortium** release.

## genieBPC 1.1.0

CRAN release: 2023-03-03

- Removed `fetch_samples()` function
  ([\#91](https://github.com/GENIE-BPC/genieBPC/issues/91))

- Updated
  [`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
  to read “NA” as NA values instead of as character “NA” so that CNA
  columns are numeric, as expected
  ([\#88](https://github.com/GENIE-BPC/genieBPC/issues/88))

- Ensure consistent order of datasets returned from calls to
  [`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)

- Added citations & links to GENIE BPC publications to README

#### Data Release Updates

- Added **PANC v1.1-consortium** release

- Added **Prostate v1.1-consortium** release

- Added **BrCa 1.2-consortium** release

- Added **PANC v1.2-consortium** release

- Added **Prostate v1.2-consortium** release

- Added **CRC 2.0-public** release

## genieBPC 1.0.1

CRAN release: 2022-10-27

- Updates to
  [`select_unique_ngs()`](https://genie-bpc.github.io/genieBPC/reference/select_unique_ngs.md)
  documentation and vignettes, as well as clarification of the messaging
  returned when the function is run
  ([\#57](https://github.com/GENIE-BPC/genieBPC/issues/57))

- Update selecting functions to account for `tidyselect` v1.2.0 release
  (issue [\#83](https://github.com/GENIE-BPC/genieBPC/issues/83))

- Documentation updates following Bioinformatics reviewer comments
  (issue [\#75](https://github.com/GENIE-BPC/genieBPC/issues/75))

- Update
  [`drug_regimen_sunburst()`](https://genie-bpc.github.io/genieBPC/reference/drug_regimen_sunburst.md)
  to pass additional arguments to
  [`sunburstR::sunburst()`](https://rdrr.io/pkg/sunburstR/man/sunburst.html)
  (issue [\#71](https://github.com/GENIE-BPC/genieBPC/issues/71))

- Added CRAN installation instructions to README (issue
  [\#72](https://github.com/GENIE-BPC/genieBPC/issues/72))

- Add CRAN badge to README (issue
  [\#69](https://github.com/GENIE-BPC/genieBPC/issues/69))

- Update handling of missing data when reading in csv files in
  [`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
  so that missing data is stored as NA and not as “” (issue
  [\#73](https://github.com/GENIE-BPC/genieBPC/issues/73)).

## genieBPC 1.0.0

CRAN release: 2022-08-14

- Changed [`print()`](https://rdrr.io/r/base/print.html) to
  [`message()`](https://rdrr.io/r/base/message.html) in
  [`select_unique_ngs()`](https://genie-bpc.github.io/genieBPC/reference/select_unique_ngs.md)

- Removed `TramineR` from dependencies file (issue
  [\#50](https://github.com/GENIE-BPC/genieBPC/issues/50))

- Removed `synapser` from dependencies file (issue
  [\#45](https://github.com/GENIE-BPC/genieBPC/issues/45))

- Fixed `cpt_sample_type`/`sample_type` issue in `fetch_samples.R`
  (issue [\#43](https://github.com/GENIE-BPC/genieBPC/issues/43))

- Added
  [`.is_connected_to_genie()`](https://genie-bpc.github.io/genieBPC/reference/dot-is_connected_to_genie.md)
  to replace `.check_synapse_login()` and return logical TRUE/FALSE.
  This is an internal function that can be used to trigger tests/checks
  as needed and will hopefully allow the package to now pass the CRAN
  checks.

- Added drug names for breast cancer cohort

- Update examples to use test data, when possible

#### Data Release Updates

- Incorporated the most recent public and consortium data releases
  (issue [\#42](https://github.com/GENIE-BPC/genieBPC/issues/42))

## genieBPC 0.1.0

- Added a `NEWS.md` file to track changes to the package.
