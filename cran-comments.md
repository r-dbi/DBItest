Updated specification

## Test environments
* local ubuntu 17.10, R 3.4.4
* ubuntu 12.04 (on travis-ci)
* win-builder (devel and release)

## R CMD check results

OK

## revdepcheck results

Checked all downstream dependencies, found a regression in the civis package. (This package is a test suite, new tests aren't accounted for yet in civis.) Notified maintainer.
