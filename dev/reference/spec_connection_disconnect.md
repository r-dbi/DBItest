# spec_connection_disconnect

spec_connection_disconnect

## Value

`dbDisconnect()` returns `TRUE`, invisibly.

## Failure modes

A warning is issued on garbage collection when a connection has been
released without calling `dbDisconnect()`, but this cannot be tested
automatically. At least one warning is issued immediately when calling
`dbDisconnect()` on an already disconnected or invalid connection.

## See also

Other connection specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
