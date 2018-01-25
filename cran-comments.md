Update that fixes RSQLite for "noLD" builds.

## Test environments
* local ubuntu 17.04, R 3.4.3
* ubuntu 12.04 (on travis-ci), R release, devel, and oldrel
* win-builder (devel and release)

## R CMD check results

OK

## revdepcheck results

Checked RSQLite, no regressions found.  Comparing to the released 1.5-1, this version contains only a very minor change to address the RSQLite check problems (skipping a test if detecting low floating-point precision), this cannot affect the functionality of the downstream dependencies.
