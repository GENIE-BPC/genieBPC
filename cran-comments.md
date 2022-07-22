## R CMD check results

* Maintainer: 'Jessica Lavery <laveryj@mskcc.org>'

* The notes from R CMD Check regarding possibly mis-spelled words in DESCRIPTION
are okay, and the words should remain as they appear.

* The examples here are taking a little over 5 seconds for the functions listed,
but are still under 10 seconds.

## Additional comments

* Re-submission of a new package

* Thank you for your first round of feedback and for your time reviewing
our package.

## Response to initial feedback

> Please always explain all acronyms in the description text. -> AACR

Thank you for catching this, we have written out "American Association for
Cancer Research" in the DESCRIPTION file.

> The maintainer should only be a single person (no mailing list)

Thank you for letting us know. We have updated the maintainer from the GENIE BPC
Statistical Core to Jessica Lavery.

> Please always write package names, software names and API (application
programming interface) names in single quotes in title and description. 
e.g: --> 'genieBPC'. 
Please note that package names are case sensitive. Please also
remove the {} around genieBPC.

We confirm that we have updated the references to the package name to comply
with the acceptable format for CRAN.

> If there are references describing the methods in your package, please add
these in the description field of your DESCRIPTION file in the form authors
(year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those
are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:'
and angle brackets for auto-linking.

Thank you. There are not yet any existing references describing the methods in
our package.

> Please add \\value to .Rd files regarding exported methods and explain the
functions results in the documentation. Please write about the structure of the
output (class) and also what the output means. (If a function does not return a
value, please document that too, e.g. \value{No return value, called for side
effects} or similar) 
Missing Rd-tags: set_synapse_credentials.Rd: \value

Thank you for pointing this out. We have added the Rd-tag to
`set_synapse_credentials()`.

> You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to extract
the information a user is interested in, and then print() that object. Instead
of print() rather use message()/warning()  or if(verbose)cat(..) (or maybe
stop()) if you really have to write text to the console. (except for print,
summary, interactive functions)
-> R/select_unique_ngs.R

We have changed instances of `print()` in `select_unique_ngs()` to `message()`
to more easily suppress the information messages.

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the package
directory and getwd()). This is not allowed by CRAN policies. Please omit any
default path in writing functions. In your examples/vignettes/tests you can
write to tempdir().

We have carefully reviewed the package and are unsure which line(s) of code
write to the user's home filespace. `pull_data_synapse()` is the only function
that is writing a file, and it utilizes `tempdir()`. If you are able to please
point us to a specific function or line number we are happy to resolve.

> Please do not modifiy the .GlobalEnv. This is not allowed by the CRAN
policies.

We have updated to not return results to the .GlobalEnv.

> Please do not set a seed to a specific number within a function. 
-> R/select_unique_ngs.R

We have removed the seed from `select_unique_ngs()`. 
