# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.0 (2017-04-21) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |Zulu                         |
|date     |2017-05-08                   |

## Packages

|package   |*  |version     |date       |source                             |
|:---------|:--|:-----------|:----------|:----------------------------------|
|DBI       |   |0.6-13      |2017-05-08 |Github (rstats-db/DBI@f6500a5)     |
|DBItest   |   |1.4-22      |2017-05-08 |Github (rstats-db/DBItest@1f344a3) |
|devtools  |*  |1.12.0.9000 |2017-05-02 |local                              |
|knitr     |   |1.15.1      |2016-11-22 |cran (@1.15.1)                     |
|lintr     |   |1.0.0       |2016-04-16 |cran (@1.0.0)                      |
|R6        |   |2.2.0       |2016-10-05 |CRAN (R 3.4.0)                     |
|rmarkdown |   |1.5         |2017-04-26 |cran (@1.5)                        |
|testthat  |   |1.0.2       |2016-04-23 |cran (@1.0.2)                      |
|withr     |   |1.0.2       |2016-06-20 |CRAN (R 3.4.0)                     |

# Check results

3 packages

|package   |version | errors| warnings| notes|
|:---------|:-------|------:|--------:|-----:|
|bigrquery |0.3.0   |      1|        0|     0|
|odbc      |1.0.1   |      0|        0|     1|
|RSQLite   |1.1-2   |      0|        0|     2|

## bigrquery (0.3.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/rstats-db/bigrquery/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘bigrquery’ can be installed ... ERROR
Installation failed.
See ‘/home/muelleki/git/R/DBItest/revdep/checks/bigrquery.Rcheck/00install.out’ for details.
```

## odbc (1.0.1)
Maintainer: Jim Hester <james.hester@rstudio.com>  
Bug reports: https://github.com/rstats-db/odbc/issues

0 errors | 0 warnings | 1 note 

```
checking compiled code ... NOTE
File ‘odbc/libs/odbc.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

## RSQLite (1.1-2)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/rstats-db/RSQLite/issues

0 errors | 0 warnings | 2 notes

```
checking installed package size ... NOTE
  installed size is  7.8Mb
  sub-directories of 1Mb or more:
    libs   7.2Mb

checking compiled code ... NOTE
File ‘RSQLite/libs/RSQLite.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

