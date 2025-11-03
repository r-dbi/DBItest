# test_data_type

test_data_type

## Usage

``` r
test_data_type(ctx, dbObj)
```

## Arguments

- ctx, dbObj:

  Arguments to internal test function

## Value

`dbDataType()` returns the SQL type that corresponds to the `obj`
argument as a non-empty character string. For data frames, a character
vector with one element per column is returned.

## Failure modes

An error is raised for invalid values for the `obj` argument such as a
`NULL` value.

## Specification

The backend can override the
[`DBI::dbDataType()`](https://dbi.r-dbi.org/reference/dbDataType.html)
generic for its driver class.

This generic expects an arbitrary object as second argument. To query
the values returned by the default implementation, run
`example(dbDataType, package = "DBI")`. If the backend needs to override
this generic, it must accept all basic R data types as its second
argument, namely [logical](https://rdrr.io/r/base/logical.html),
[integer](https://rdrr.io/r/base/integer.html),
[numeric](https://rdrr.io/r/base/numeric.html),
[character](https://rdrr.io/r/base/character.html), dates (see
[Dates](https://rdrr.io/r/base/Dates.html)), date-time (see
[DateTimeClasses](https://rdrr.io/r/base/DateTimeClasses.html)), and
[difftime](https://rdrr.io/r/base/difftime.html). If the database
supports blobs, this method also must accept lists of
[raw](https://rdrr.io/r/base/raw.html) vectors, and
[blob::blob](https://blob.tidyverse.org/reference/blob.html) objects.
As-is objects (i.e., wrapped by
[`I()`](https://rdrr.io/r/base/AsIs.html)) must be supported and return
the same results as their unwrapped counterparts. The SQL data type for
[factor](https://rdrr.io/r/base/factor.html) and
[ordered](https://rdrr.io/r/base/factor.html) is the same as for
character. The behavior for other object types is not specified.
