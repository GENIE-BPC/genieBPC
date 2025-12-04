# Check download_path user passed and create folder if needed

Check download_path user passed and create folder if needed

## Usage

``` r
.check_download_path(download_location, version_num)
```

## Arguments

- download_location:

  a local path or NULL

- version_num:

  vector of cohort/version_number

## Value

a vector of file paths. If download_location is NULL, will return
temporary file path

## Examples

``` r
.check_download_path(download_location = NULL, version_num = "CRC_v2.1")
#> [1] "/tmp/RtmplxS5NW/CRC_v2.1"
```
