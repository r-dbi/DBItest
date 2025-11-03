# DBItest: Testing DBI Backends

A helper that tests DBI back ends for conformity to the interface.

## Details

The two most important functions are
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
and [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md).
The former tells the package how to connect to your DBI backend, the
latter executes all tests of the test suite. More fine-grained test
functions (all with prefix `test_`) are available.

See the package's vignette for more details.

## See also

Useful links:

- <https://dbitest.r-dbi.org>

- <https://github.com/r-dbi/DBItest>

- Report bugs at <https://github.com/r-dbi/DBItest/issues>

## Author

Kirill Müller
