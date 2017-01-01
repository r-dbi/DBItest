# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |Universal                    |
|date     |2016-12-02                   |

## Packages

|package   |*  |version     |date       |source                             |
|:---------|:--|:-----------|:----------|:----------------------------------|
|DBI       |   |0.5-12      |2016-12-02 |Github (rstats-db/DBI@562d091)     |
|DBItest   |   |1.3-11      |2016-12-02 |Github (rstats-db/DBItest@d89656a) |
|devtools  |*  |1.12.0.9000 |2016-10-06 |local                              |
|knitr     |   |1.15.1      |2016-11-22 |cran (@1.15.1)                     |
|lintr     |   |1.0.0       |2016-04-16 |cran (@1.0.0)                      |
|rmarkdown |   |1.2         |2016-11-21 |cran (@1.2)                        |
|testthat  |   |1.0.2.9000  |2016-08-25 |Github (hadley/testthat@46d15da)   |
|withr     |   |1.0.2       |2016-06-20 |CRAN (R 3.3.1)                     |

# Check results

1 packages with problems

|package |version | errors| warnings| notes|
|:-------|:-------|------:|--------:|-----:|
|RSQLite |1.1     |      1|        0|     1|

## RSQLite (1.1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/rstats-db/RSQLite/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1/1 mismatches
  [1] 1 - 0 == 1
  
  
  testthat results ================================================================
  OK: 1214 SKIPPED: 10 FAILED: 2
  1. Failure: DBItest[RSQLite]: Driver: stress_load_unload (@test-driver.R#139) 
  2. Failure: DBItest[RSQLite]: Connection: stress_load_connect_unload (@test-connection.R#152) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  Unknown tweaks: placeholder_pattern 
  Execution halted

checking installed package size ... NOTE
  installed size is  7.8Mb
  sub-directories of 1Mb or more:
    libs   7.2Mb
```

