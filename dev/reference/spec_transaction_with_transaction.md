# spec_transaction_with_transaction

spec_transaction_with_transaction

## Value

`dbWithTransaction()` returns the value of the executed code.

## Failure modes

Failure to initiate the transaction (e.g., if the connection is closed
or invalid or if
[`dbBegin()`](https://dbi.r-dbi.org/reference/transactions.html) has
been called already) gives an error.

## Specification

`dbWithTransaction()` initiates a transaction with `dbBegin()`, executes
the code given in the `code` argument, and commits the transaction with
[`dbCommit()`](https://dbi.r-dbi.org/reference/transactions.html). If
the code raises an error, the transaction is instead aborted with
[`dbRollback()`](https://dbi.r-dbi.org/reference/transactions.html), and
the error is propagated. If the code calls `dbBreak()`, execution of the
code stops and the transaction is silently aborted. All side effects
caused by the code (such as the creation of new variables) propagate to
the calling environment.

## See also

Other transaction specifications:
[`spec_transaction_begin_commit_rollback`](https://dbitest.r-dbi.org/dev/reference/spec_transaction_begin_commit_rollback.md)
