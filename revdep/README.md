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
|date     |2017-06-18                   |

## Packages

|package   |*  |version |date       |source                         |
|:---------|:--|:-------|:----------|:------------------------------|
|DBI       |   |0.6-14  |2017-06-17 |Github (rstats-db/DBI@0f390df) |
|DBItest   |   |1.4     |2016-12-03 |CRAN (R 3.4.0)                 |
|devtools  |*  |1.13.2  |2017-06-02 |cran (@1.13.2)                 |
|knitr     |   |1.16    |2017-05-18 |cran (@1.16)                   |
|lintr     |   |1.0.0   |2016-04-16 |cran (@1.0.0)                  |
|R6        |   |2.2.2   |2017-06-17 |cran (@2.2.2)                  |
|rmarkdown |   |1.6     |2017-06-15 |cran (@1.6)                    |
|testthat  |   |1.0.2   |2016-04-23 |cran (@1.0.2)                  |
|withr     |   |1.0.2   |2016-06-20 |CRAN (R 3.4.0)                 |

# Check results

4 packages

|package    |version | errors| warnings| notes|
|:----------|:-------|------:|--------:|-----:|
|bigrquery  |0.3.0   |      1|        0|     0|
|odbc       |1.0.1   |      0|        0|     1|
|RSQLite    |1.1-2   |      0|        0|     2|
|RSQLServer |0.3.0   |      0|        0|     0|

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
  installed size is  8.0Mb
  sub-directories of 1Mb or more:
    libs   7.4Mb

checking compiled code ... NOTE
File ‘RSQLite/libs/RSQLite.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

## RSQLServer (0.3.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>  
Bug reports: https://github.com/imanuelcostigan/RSQLServer/issues

0 errors | 0 warnings | 0 notes

