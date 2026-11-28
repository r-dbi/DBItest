# reverse order

# Script to create new spec files from subspec names read from clipboard:
# pbpaste | gsed 's/,//' | for i in $(cat); do f=$(echo $i | gsed 's/_/-/g;s/$/.R/'); echo "$i <- list(" > R/$f; echo ")" >> R/$f; echo "#' @include $f"; done | tac | pbcopy
#
# Example input:
# test_xxx_1,
# test_xxx_2,
#
# Output: Files R/test-xxx-1.R and R/test-xxx-2.R, and @include directives to stdout

##### All
#' @include spec-all.R
##### Stress
#' @include spec-stress.R
#' @include spec-stress-connection.R
##### Aggregators
#' @include spec-compliance.R
#' @include spec-transaction.R
#' @include spec-arrow.R
#' @include spec-meta.R
#' @include spec-sql.R
#' @include spec-result.R
#' @include spec-connection.R
#' @include spec-driver.R
##### Later
#' @include spec-meta-get-info-result.R
#' @include spec-meta-column-info.R
#' @include spec-sql-list-fields.R
#' @include spec-connection-get-info.R
#' @include spec-driver-get-info.R
##### Arrow
#' @include spec-arrow-roundtrip.R
#' @include spec-arrow-bind.R
#' @include spec-arrow-append-table-arrow.R
#' @include spec-arrow-create-table-arrow.R
#' @include spec-arrow-write-table-arrow.R
#' @include spec-arrow-read-table-arrow.R
#' @include spec-arrow-get-query-arrow.R
#' @include spec-arrow-fetch-arrow-chunk.R
#' @include spec-arrow-fetch-arrow.R
#' @include spec-arrow-send-query-arrow.R
##### Method specs
#' @include spec-transaction-with-transaction.R
#' @include spec-transaction-begin-commit-rollback.R
#' @include spec-meta-get-rows-affected.R
#' @include spec-meta-get-row-count.R
#' @include spec-meta-get-statement.R
#' @include spec-meta-has-completed.R
#' @include spec-meta-is-valid.R
#' @include spec-meta-bind-.R
#' @include spec-meta-bind-arrow-stream.R
#' @include spec-meta-bind-stream.R
#' @include spec-meta-bind-arrow.R
#' @include spec-meta-bind.R
#' @include spec-meta-bind-expr.R
#' @include spec-meta-bind-formals.R
#' @include spec-meta-bind-runner.R
#' @include spec-sql-list-objects.R
#' @include spec-sql-remove-table.R
#' @include spec-sql-exists-table.R
#' @include spec-sql-list-tables.R
#' @include spec-sql-write-table.R
#' @include spec-sql-append-table.R
#' @include spec-sql-create-table.R
#' @include spec-sql-read-table.R
#' @include spec-sql-unquote-identifier.R
#' @include spec-sql-quote-identifier.R
#' @include spec-sql-quote-literal.R
#' @include spec-sql-quote-string.R
#' @include spec-result-execute.R
#' @include spec-result-send-statement.R
#' @include spec-result-get-query.R
#' @include spec-result-clear-result.R
#' @include spec-result-roundtrip.R
#' @include spec-result-fetch.R
#' @include spec-result-send-query.R
#' @include spec-connection-disconnect.R
#' @include spec-driver-connect.R
#' @include spec-result-create-table-with-data-type.R
#' @include spec-connection-data-type.R
#' @include spec-driver-data-type.R
##### Soft specs
#' @include spec-driver-constructor.R
#' @include spec-compliance-methods.R
#' @include spec-getting-started.R
NULL
