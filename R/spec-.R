# reverse order

# Script to create new spec files from subspec names read from clipboard:
# xclip -out -se c | sed 's/,//' | for i in $(cat); do f=$(echo $i | sed 's/_/-/g;s/$/.R/'); echo "$i <- list(" > R/$f; echo ")" >> R/$f; echo "#' @include $f"; done | tac
#
# Example input:
# test_xxx_1,
# test_xxx_2,
#
# Output: Files R/test-xxx-1.R and R/test-xxx-2.R, and @include directives to stdout

#' @include spec-stress.R
#' @include spec-stress-connection.R
#' @include spec-stress-driver.R
#' @include spec-compliance.R
#' @include spec-compliance-read-only.R
#' @include spec-compliance-methods.R
#' @include spec-transaction.R
#' @include spec-transaction-with-transaction.R
#' @include spec-transaction-begin-rollback.R
#' @include spec-transaction-begin-commit.R
#' @include spec-meta.R
#' @include spec-meta-bind-.R
#' @include spec-meta-bind-multi-row.R
#' @include spec-meta-bind.R
#' @include spec-meta-get-info-result.R
#' @include spec-meta-get-rows-affected.R
#' @include spec-meta-get-row-count.R
#' @include spec-meta-column-info.R
#' @include spec-meta-get-statement.R
#' @include spec-meta-is-valid-result.R
#' @include spec-meta-is-valid-connection.R
#' @include spec-sql.R
#' @include spec-sql-list-fields.R
#' @include spec-sql-list-tables.R
#' @include spec-sql-read-write-roundtrip.R
#' @include spec-sql-read-write-table.R
#' @include spec-sql-quote-identifier.R
#' @include spec-sql-quote-string.R
#' @include spec-result.R
#' @include spec-result-roundtrip.R
#' @include spec-result-create-table-with-data-type.R
#' @include spec-result-get-query.R
#' @include spec-result-fetch.R
#' @include spec-result-send-query.R
#' @include spec-connection.R
#' @include spec-connection-get-info.R
#' @include spec-connection-data-type.R
#' @include spec-connection-connect.R
#' @include spec-driver.R
#' @include spec-driver-get-info.R
#' @include spec-driver-data-type.R
#' @include spec-driver-constructor.R
#' @include spec-driver-class.R
#' @include spec-getting-started.R
#' @include spec.R
NULL
