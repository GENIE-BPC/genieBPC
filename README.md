# GenieBPC

The {GenieBPC} package provides a seamless way to obtain the data corresponding to each release from Synapse and to create datasets for analysis.

* **Pull a specified version of a phenomic data release** from Synapse directly into the R environment. The most recent version is automatically pulled, but a specific version can be specified for the purposes of reproducing analyses. The phenomic data are recorded in multiple datasets, including: patient characteristics (vital status), cancer diagnosis, cancer-directed drug regimens, pathology, imaging, medical oncologist assessment, tumor markers and cancer panel test. 

* **Create a cohort for analysis** based on specified diagnosis or regimen inclusion criteria. 

* **Pull GENIE genomic data** corresponding to the analytic cohort directly into the R environment, including selection of a single next-generation sequencing test in the case that the patient has multiple, based on user-specified criteria.

The datasets obtained through {GenieBPC} can be input directly into {gnomeR} to prepare the genomics data for analysis. {gnomeR} annotates the genomic data ... [Axel to add]

## Installation

You can install the development version of {GenieBPC} with the following code:

``` r
remotes::install_github("AxelitoMartin/GenieBPC")
```

## Examples
