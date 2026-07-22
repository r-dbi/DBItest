# Create a function that creates n placeholders

For internal use by the `placeholder_format` tweak.

## Usage

``` r
make_placeholder_fun(pattern)
```

## Arguments

- pattern:

  `[character(1)]`  
  Any character, optionally followed by `1` or `name`. Examples: `"?"`,
  `"$1"`, `":name"`

## Value

`[function(n)]`  
A function with one argument `n` that returns a vector of length `n`
with placeholders of the specified format.

## Examples

``` r
body(DBItest:::make_placeholder_fun("?"))
#> rep("?", n)
DBItest:::make_placeholder_fun("?")(2)
#> [1] "?" "?"
DBItest:::make_placeholder_fun("$1")(3)
#> [1] "$1" "$2" "$3"
DBItest:::make_placeholder_fun(":name")(5)
#>    a    b    c    d    e 
#> ":a" ":b" ":c" ":d" ":e" 
```
