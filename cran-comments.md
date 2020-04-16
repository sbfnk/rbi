## Test environments
* local macOS install (10.14.5), R 3.6.3
* local linux install (Ubuntu 18.04), R 3.6.3
* macOS and Linux on travis-ci
* checked with devtools::check_rhub()
* checked with devtools::check_win_oldrelease()
* checked with devtools::check_win_release()
* checked with devtools::check_win_devel()

## R CMD check results
There were no ERRORs or WARNINGs.

There was a NOTE that this is a new submission as a previous version was removed from CRAN. All problems with that version should be fixed now.

## Downstream dependencies
0 packages with problems.

## Static vignette
A vignette is included, built locally using R.rsp as it depends on external software (LibBi) listed in the SystemRequirements.

## Responses to CRAN comments

* Redundant R in title removed
* package names now in quotation marks
* return values added to the documentation of all functions
* \dontrun replaced by \donttest
* nothing is printed directly to the console unless explicitly requested
* nothing is written to user filespace
* no user option are set
