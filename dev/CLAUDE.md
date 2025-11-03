# Claude Development Guidelines for DBItest

## Project Overview

DBItest is a comprehensive testing framework for database interface
(DBI) implementations in R. The package provides standardized compliance
tests to ensure DBI implementations work correctly across different
database backends. This document provides guidelines for AI assistants
working on this codebase.

## Code Style and Documentation

### Comment Style

- Add comprehensive comments to utility functions that aren’t
  immediately obvious
- Use line breaks after each sentence in multi-sentence comments
- Focus comments on explaining the “why” and “how”, the “what” should be
  clear from the code itself
- Include context about the function’s role in the testing framework

### R Code Conventions

- Follow existing naming conventions (snake_case for functions,
  camelCase for some legacy functions)
- Use explicit package prefixes (e.g.,
  [`withr::local_db_connection`](https://withr.r-lib.org/reference/with_db_connection.html))
  for clarity
- Maintain consistent indentation and spacing patterns
- Use meaningful variable names that reflect the testing context

## Install and run R

Never check if rig or R are installed on GitHub Actions, they are not.
Use rig to install R. Install rig with:

``` bash
curl -Ls https://github.com/r-lib/rig/releases/download/latest/rig-linux-$(arch)-latest.tar.gz |
  sudo tar xz -C /usr/local
```

Then install the current release version of R:

``` bash
rig add release
```

Run R code with:

``` bash
Rscript -e '<expression>'
```

Use pak to install packages, it will be installed by rig:

``` r
pak::pak("<package>")
```
