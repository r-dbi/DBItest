## Test environments
* ubuntu 14.04, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Missing or unexported object: 'DBI::dbBind'

  This virtual method is available only in the development release of the DBI
  package, which hasn't been published to CRAN yet..

## Downstream dependencies
Initial submission.
