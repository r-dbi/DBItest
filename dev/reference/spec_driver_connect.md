# spec_driver_connect

spec_driver_connect

## Value

`dbConnect()` returns an S4 object that inherits from
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
This object is used to communicate with the database engine.

A [`format()`](https://rdrr.io/r/base/format.html) method is defined for
the connection object. It returns a string that consists of a single
line of text.

## Specification

DBI recommends using the following argument names for authentication
parameters, with `NULL` default:

- `user` for the user name (default: current user)

- `password` for the password

- `host` for the host name (default: local connection)

- `port` for the port number (default: local connection)

- `dbname` for the name of the database on the host, or the database
  file name

The defaults should provide reasonable behavior, in particular a local
connection for `host = NULL`. For some DBMS (e.g., PostgreSQL), this is
different to a TCP/IP connection to `localhost`.

In addition, DBI supports the `bigint` argument that governs how 64-bit
integer data is returned. The following values are supported:

- `"integer"`: always return as `integer`, silently overflow

- `"numeric"`: always return as `numeric`, silently round

- `"character"`: always return the decimal representation as `character`

- `"integer64"`: return as a data type that can be coerced using
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (with warning on
  overflow), [`as.numeric()`](https://rdrr.io/r/base/numeric.html) and
  [`as.character()`](https://rdrr.io/r/base/character.html)

## See also

Other driver specifications:
[`spec_driver_constructor`](https://dbitest.r-dbi.org/dev/reference/spec_driver_constructor.md),
[`spec_driver_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_driver_data_type.md),
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
