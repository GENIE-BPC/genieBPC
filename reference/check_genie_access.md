# Check Access to GENIE Data

Check Access to GENIE Data

## Usage

``` r
check_genie_access(pat = NULL, check_consortium_access = FALSE)
```

## Arguments

- pat:

  'Synapse' Personal Access Token. If NULL, package will search package
  environment for "pat".

- check_consortium_access:

  Specifies whether access to GENIE BPC consortium data releases (vs.
  public data releases) is checked. Default is FALSE, indicating that
  access to GENIE BPC public data releases is checked instead.

## Value

A success message if you are able to access GENIE BPC data; otherwise an
error

## Author

Karissa Whiting

## Examples

``` r
if (FALSE) { # \dontrun{
# if credentials are saved:
check_genie_access()
} # }
```
