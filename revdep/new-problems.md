# civis

Version: 1.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      21: httr::authenticate(api_key(), "")
      22: stopifnot(is.character(user), length(user) == 1) at /tmp/RtmpPN5loB/R.INSTALL1e622255b5a6f/httr/R/authenticate.r:18
      23: api_key()
      24: stop("The environmental variable CIVIS_API_KEY is not set. Add this to your ", ".Renviron or call Sys.setenv(CIVIS_API_KEY = '<api_key>')")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 982 SKIPPED: 4 FAILED: 5
      1. Error: DBItest: Driver: connect_format (@spec-driver-connect.R#21) 
      2. Error: DBItest: Driver: connect_bigint_integer (@spec-driver-connect.R#52) 
      3. Error: DBItest: Driver: connect_bigint_numeric (@spec-driver-connect.R#62) 
      4. Error: DBItest: Driver: connect_bigint_character (@spec-driver-connect.R#72) 
      5. Error: DBItest: Driver: connect_bigint_integer64 (@spec-driver-connect.R#85) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

