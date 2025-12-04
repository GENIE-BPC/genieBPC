# Retrieve a synapse token using PAT

Retrieve a synapse token using PAT

## Usage

``` r
.get_synapse_token(pat = NULL)
```

## Arguments

- pat:

  'Synapse' Personal Access Token. If NULL, package will search package
  environment for "pat".

## Value

a 'Synapse' token. Will look for PAT and return that if successful HTTP
call

## Examples

``` r
if (FALSE) { # \dontrun{
.get_synapse_token(pat = "test")
} # }
```
