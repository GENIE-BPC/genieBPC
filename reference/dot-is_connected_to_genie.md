# Check Synapse Login Status & Ability to Access Data

The \`.is_connected_to_genie()\` function assesses whether the user is
logged into 'Synapse' and confirms whether the user has permission to
access the GENIE BPC data. It returns TRUE or FALSE

## Usage

``` r
.is_connected_to_genie(pat = NULL, check_consortium_access = FALSE)
```

## Value

Returns message indicating user is logged into 'Synapse' and has
permission to access the GENIE BPC data.

## Examples

``` r
.is_connected_to_genie()
#> [1] TRUE
```
