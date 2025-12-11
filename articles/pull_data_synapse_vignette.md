# Tutorial: pull_data_synapse

## Introduction

The
[`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
function accesses the specified version of the clinical and genomic
GENIE BPC data from
[Synapse](https://www.synapse.org/#!Synapse:syn21226493/wiki/599164) and
reads it into the R environment.

This vignette will walk a user through the
[`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
function.

## Setup

Access to the GENIE BPC data release folders on ‘Synapse’ is required in
order to use this function. To obtain access:

***For public data releases:***

1.  Register for a [‘Synapse’ account](https://www.synapse.org/#). Be
    sure to create a username and password. *Note: do NOT connect via
    your Google account.*

2.  *New July 2025* Enable two-factor authentication on Synapse. This
    step is required for accessing the data via {genieBPC}. You will not
    need to use two-factor authentication each time you pull the data.
    However, it is required that you enable two-factor authentication to
    allow your Personal Access Token (PAT) to successfully pull the data
    via {genieBPC}.

3.  Accept the **Synapse account terms of use.**

4.  Navigate to [GENIE Biopharma Collaborative Public
    page](https://www.synapse.org/Synapse:syn27056172/wiki/616601).

5.  In the Files folder, navigate to Data Releases and click on the
    cancer cohort and data release version of choice.

6.  Select *Request Access*, **review the terms of data use,** and
    select *Accept.*

***For consortium data releases (restricted to GENIE consortium members
& BPC pharmaceutical partners):***

1.  Register for a [‘Synapse’ account](https://www.synapse.org/#). Be
    sure to create a username and password. *Note: do NOT connect via
    your Google account.*

2.  *New July 2025* Enable two-factor authentication on Synapse. This
    step is required for accessing the data via {genieBPC}. You will not
    need to use two-factor authentication each time you pull the data.
    However, it is required that you enable two-factor authentication to
    allow your Personal Access Token (PAT) to successfully pull the data
    via {genieBPC}.

3.  Accept the **Synapse account terms of use.**

4.  Email <genieinfo@aacr.org> with your full name and affiliation to
    request a concept sheet. Once the concept sheet has been reviewed
    and approval has been granted by the Data Use and Publications
    Committee, access to the consortium data on Synapse will then be
    assigned to that user. *(Note: Please allow up to a week to review
    and grant access.)*

5.  Once the request is accepted, you may access the data in the [GENIE
    Biopharma Collaborative
    projects](https://www.synapse.org/#!Synapse:syn21226493).

6.  In the Files folder, navigate to Data Releases and click on the
    cancer cohort and data release version of choice.

7.  Select *Request Access*, **review the terms of data use,** and
    select *Accept.*

*Note: permissions for Synapse and permissions for each data release are
distinct. Both permissions must be accepted to successfully access the
data.*

***Request a Synapse Personal Access Token (PAT):***

Once you are logged into your Synapse account, you may request a Synapse
personal access token (PAT). As of July 2025, this is the only method
that permits accessing the data via the {genieBPC} R package.
Previously, username and passwords were also accepted.

1.  In the left hand panel towards the bottom, click the button that
    shows the first letter of your first name. This button displays the
    text “Your Account” when hovered over and appears directly above the
    question mark button.

2.  Next, select *Account Settings* and then scroll to the bottom of the
    page.

3.  Under the “Personal Access Tokens” section, select *Manage Personal
    Access Tokens.*

4.  On the “Personal Access Tokens” page, click on *Create New Token* to
    generate a Synapse Personal Access Token.

5.  Specify a Token Name and click the checkbox that will allow you to
    *Download* the data, and then select *Create Token.*

6.  Save your Synapse Personal Access Token in a secure location.

***Authenticate yourself***

Whether you are using public or consortium data, you will need to
authenticate yourself at the beginning of each R session in which you
use {genieBPC} to pull data. To set your Synapse Personal Access Token
(PAT) during each session, call:

`set_synapse_credentials(pat = "your_pat")`

- If your credentials are stored as an environmental variable, you do
  not need to call
  [`set_synapse_credentials()`](https://genie-bpc.github.io/genieBPC/reference/set_synapse_credentials.md)
  explicitly each session. To store authentication information in your
  environmental variables, add the following to your .Renviron file,
  then **restart your R session** (tip: you can use
  `usethis::edit_r_environ()` to easily open/edit this file):

  `SYNAPSE_PAT = <your-pat>`

Alternatively, you can pass your PAT to each individual data pull
function if preferred, although it is recommended that you manage your
PATs outside of your scripts for security purposes.

## Usage

Let’s start by reviewing the
[`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
arguments.

| Argument            | Description                                                                                                                                                                                                                                                                                                                                                                              |
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `cohort`            | Vector or list specifying the cohort(s) of interest. Must be one of 'NSCLC' (Non-Small Cell Lung Cancer), 'CRC' (Colorectal Cancer), 'BrCa' (Breast Cancer), 'BLADDER' (Bladder Cancer), 'PANC' (Pancreatic Cancer), 'Prostate' (Prostate Cancer), or 'RENAL' (Renal Cancer).                                                                                                            |
| `version`           | Vector specifying the version of the data. Must be one of the following: 'v1.1-consortium', 'v1.2-consortium', 'v2.1-consortium', 'v2.0-public', 'v3.1-consortium'. When entering multiple cohorts, the order of the version numbers corresponds to the order that the cohorts are specified; the cohort and version number must be in the same order in order to pull the correct data. |
| `download_location` | If \`NULL\` (default), data will be returned as a list of dataframes with requested data as list items. Otherwise, specify a folder path to have data automatically downloaded there. When a path is specified, data are not read into the R environment.                                                                                                                                |
| `PAT`               | Synapse Personal Access Token                                                                                                                                                                                                                                                                                                                                                            |

## Examples

### Example 1

Pull version 2.0-public of the NSCLC data from Synapse and store in the
local environment.

``` r
nsclc_2_0 <- pull_data_synapse("NSCLC", version = "v2.0-public")
```

The resulting `nsclc_2_0` object is a list of elements, such that each
element represents a dataset:

### Example 2

Pull version 2.2-consortium of the NSCLC data and version 1.3-consortium
of the CRC data.

``` r
pull_data_synapse(c("NSCLC", "CRC"), 
                  version = c("v2.2-consortium","v1.3-consortium"))
```

### Example 3

Pull version 1.2-consortium of the NSCLC data and version 1.1-consortium
of the CRC data.

``` r
pull_data_synapse(c("NSCLC", "CRC"),
                  version = c("v1.2-consortium", "v1.1-consortium"))
```

## Future Work

- As of December 2025, all seven cancer cohorts — non-small cell lung
  cancer (NSCLC), colorectal cancer (CRC), breast cancer (BrCa),
  pancreatic cancer (PANC), prostate cancer (Prostate), bladder cancer
  (BLADDER), and renal cancer (RENAL) — have undergone consortium data
  releases. Additionally, the NSCLC, CRC, BrCa cohorts have been
  released publicly. Future public data releases for the remaining
  cohorts are planned.

## Differences Between Synapse and cBioPortal Genomic Data

Please note that pulling genomic GENIE data from Synapse using
[`pull_data_synapse()`](https://genie-bpc.github.io/genieBPC/reference/pull_data_synapse.md)
and pulling GENIE data from cBioPortal may result in small differences
in the data due to systematic differences in the processing pipelines
employed by Synapse and cBioPortal. These differences may include:

- Data formatting - Some data sets (e.g. CNA files) may appear in wide
  format in Synapse data versus long format in cBioPortal data, or
  column attributes and names may appear sightly different (e.g. fusions
  files).

- Default filtering - By default, cBioPortal filters out Silent, Intron,
  IGR, 3’UTR, 5’UTR, 3’Flank and 5’Flank, except for the promoter
  mutations of the TERT gene. See [cBioPortal
  documentation](https://docs.cbioportal.org/file-formats/#mutation-data)
  for more details. These mutations are retained in Synapse processing
  pipelines.

- Hugo Symbols - Some genes have more than one accepted Hugo Symbol and
  may be referred to differently between data sources (e.g. `NSD3` is an
  alias for `WHSC1L1`). Some tools exist to help you resolve gene
  aliases across genomic data sources. See `gnomeR::recode_alias()`,
  `cbioportal::get_alias()` and vignettes from the
  [{gnomeR}](https://mskcc-epi-bio.github.io/gnomeR/) and
  [{cbioportalR}](https://www.karissawhiting.com/cbioportalR/) for more
  information on how to use these functions and work with gene aliases.
