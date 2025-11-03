# spec_transaction_begin_commit_rollback

spec_transaction_begin_commit_rollback

## Value

`dbBegin()`, `dbCommit()` and `dbRollback()` return `TRUE`, invisibly.

## Failure modes

The implementations are expected to raise an error in case of failure,
but this is not tested. In any way, all generics throw an error with a
closed or invalid connection. In addition, a call to `dbCommit()` or
`dbRollback()` without a prior call to `dbBegin()` raises an error.
Nested transactions are not supported by DBI, an attempt to call
`dbBegin()` twice yields an error.

## Specification

Actual support for transactions may vary between backends. A transaction
is initiated by a call to `dbBegin()` and committed by a call to
`dbCommit()`. Data written in a transaction must persist after the
transaction is committed. For example, a record that is missing when the
transaction is started but is created during the transaction must exist
both during and after the transaction, and also in a new connection.

A transaction can also be aborted with `dbRollback()`. All data written
in such a transaction must be removed after the transaction is rolled
back. For example, a record that is missing when the transaction is
started but is created during the transaction must not exist anymore
after the rollback.

Disconnection from a connection with an open transaction effectively
rolls back the transaction. All data written in such a transaction must
be removed after the transaction is rolled back.

The behavior is not specified if other arguments are passed to these
functions. In particular, RSQLite issues named transactions with support
for nesting if the `name` argument is set.

The transaction isolation level is not specified by DBI.

## See also

Other transaction specifications:
[`spec_transaction_with_transaction`](https://dbitest.r-dbi.org/dev/reference/spec_transaction_with_transaction.md)
