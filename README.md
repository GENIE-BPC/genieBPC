# genieBPC

The {genieBPC} package provides a seamless way to obtain the data corresponding to each release from Synapse and to create datasets for analysis.

* **Pull a specified version of a phenomic data release** from Synapse directly into the R environment. The most recent version is automatically pulled, but a specific version can be specified for the purposes of reproducing analyses. The phenomic data are recorded in multiple datasets, including: patient characteristics (vital status), cancer diagnosis, cancer-directed drug regimens, pathology, imaging, medical oncologist assessment, tumor markers and cancer panel test. 

* **Create a cohort for analysis** based on specified diagnosis or regimen inclusion criteria. 

* **Pull GENIE genomic data** corresponding to the analytic cohort directly into the R environment, including selection of a single next-generation sequencing test in the case that the patient has multiple, based on user-specified criteria.

The datasets obtained through {GenieBPC} can be input directly into {gnomeR} to prepare the genomics data for analysis. {gnomeR} annotates the genomic data ... [Axel to add]

## Installation

You can install the development version of {genieBPC} with the following code:

``` r
remotes::install_github("AxelitoMartin/genieBPC")
```

## Obtaining Data Access

Access to the GENIE BPC data release folders on Synapse is required in order to use this function. To obtain access:
1. Register for a [Synapse account](https://www.synapse.org/#!Reigster0)
2. Use [this link](https://www.synapse.org/#!Team:3399797) to access the GENIE BPC team list and request to join the team. Please include your full name and affiliation in the message before sending out the request.
3. Once the request is accepted, you may access the data in the [GENIE Biopharma collaborative projects](https://www.synapse.org/#!Synapse:syn21226493).

*Note: Please allow up to a week to review and grant access.*

## Examples

## Additional References 

Placeholder for GENIE BPC publications
