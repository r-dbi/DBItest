# spec_driver_constructor

spec_driver_constructor

## Construction of the DBIDriver object

The backend must support creation of an instance of its
[DBI::DBIDriver](https://dbi.r-dbi.org/reference/DBIDriver-class.html)
subclass with a constructor function. By default, its name is the
package name without the leading ‘R’ (if it exists), e.g., `SQLite` for
the RSQLite package. However, backend authors may choose a different
name. The constructor must be exported, and it must be a function that
is callable without arguments. DBI recommends to define a constructor
with an empty argument list.

## See also

Other driver specifications:
[`spec_driver_connect`](https://dbitest.r-dbi.org/dev/reference/spec_driver_connect.md),
[`spec_driver_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_driver_data_type.md),
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
