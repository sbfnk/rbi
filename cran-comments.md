## Test environments
* local macOS install (12.0.1), R 4.1.2
* macOS and Linux on travis-ci
* checked with devtools::check_rhub()
* checked with devtools::check_win_oldrelease()
* checked with devtools::check_win_release()
* checked with devtools::check_win_devel()

## R CMD check results
There were no ERRORs or WARNINGs, or NOTEs.

## Downstream dependencies
0 packages with problems.

## Static vignette
A vignette is included, built locally using R.rsp as it depends on external software (LibBi) listed in the SystemRequirements.

## Responses to CRAN comments

Thank you for the review. We have:
* removed redundant R in title
* put package names in quotation marks
* added return values to the documentation of all functions
* replaced \dontrun by \donttest
* reviewed the code to check nothing is printed directly to the console unless explicitly requested
* reviewed the code to check nothing is written to user filespace
* changed the code so no user option are set
