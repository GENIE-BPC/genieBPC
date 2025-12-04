# Connect to 'Synapse' API

This function sets 'Synapse' credentials for the user's current session.

## Usage

``` r
set_synapse_credentials(pat = NULL)
```

## Arguments

- pat:

  'Synapse' Personal Access Token. If NULL, package will search
  environmental variables for \`SYNAPSE_PAT\`.

## Value

A success message if you credentials are valid for 'Synapse' platform;
otherwise an error

## Details

To access data, users must have a valid 'Synapse' account with
permission to access the data set and they must have accepted any
necessary 'Terms of Use'. Users must authenticate themselves in their
current R session. (See README 'Data Access and Authentication' at
https://genie-bpc.github.io/genieBPC/ for details). To set your
'Synapse' Personal Access Token (PAT) during each session, call:
\`set_synapse_credentials(pat = "your_pat")\`.

If your credentials are stored as an environmental variable, you do not
need to call \`set_synapse_credentials()\` explicitly each session. To
store authentication information in your environmental variables, add
the following to your .Renviron file, then restart your R session (tip:
you can use \`usethis::edit_r_environ()\` to easily open/edit this
file):

- \`SYNAPSE_PAT = \<your-pat\>\`

Alternatively, you can pass your PAT to each individual data pull
function if preferred, although it is recommended that you manage your
PATs outside of your scripts for security purposes.

## Author

Karissa Whiting

## Examples

``` r
if (FALSE) { # \dontrun{
set_synapse_credentials(
  pat = "your-personal-access-token"
)
} # }
```
