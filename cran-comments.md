## Test environments
* local ubuntu 17.04, R 3.4.0
* ubuntu 12.04 (on travis-ci), R release, devel, and oldrel
* win-builder (devel and release)

## R CMD check results

OK

## Reverse dependencies

* I checked the four reverse dependencies. RSQLite requires an update which
  I have submitted simultaneously, for the other three I haven't noticed any
  regressions.
