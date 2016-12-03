## Test environments
* local ubuntu 16.10, R 3.3.2
* ubuntu 12.04 (on travis-ci), R release, devel, and oldrel
* win-builder (devel and release)

## R CMD check results

Changes to the package's description as requested by Uwe Ligges. I hope a same-version update is okay here.

## Reverse dependencies

* I checked the two reverse dependencies `bigrquery` and `RSQLite`.
  There were no ERRORs, WARNINGs or NOTEs.

* RSQLite 1.1 on OS X seems to depend on this release, could you please trigger
  a rebuild of RSQLite 1.1 once DBItest 1.4 has been built for OS X?
