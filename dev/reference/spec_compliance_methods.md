# spec_compliance_methods

spec_compliance_methods

## DBI classes and methods

A backend defines three classes, which are subclasses of
[DBIDriver](https://dbi.r-dbi.org/reference/DBIDriver-class.html),
[DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html),
and [DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html).
The backend provides implementation for all methods of these base
classes that are defined but not implemented by DBI. All methods defined
in DBI are reexported (so that the package can be used without having to
attach DBI), and have an ellipsis `...` in their formals for
extensibility.
