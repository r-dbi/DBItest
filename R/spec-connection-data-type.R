#' @template dbispec-sub-wip
#' @format NULL
#' @section Connection:
#' \subsection{`dbDataType("DBIConnection", "ANY")`}{
spec_connection_data_type <- list(
  #' SQL Data types exist for all basic R data types. dbDataType() does not
  #' throw an error and returns a nonempty atomic character
  data_type_connection = function(ctx) {
    con <- connect(ctx)
    check_conn_data_type <- function(value) {
      eval(bquote({
        expect_is(dbDataType(con, .(value)), "character")
        expect_equal(length(dbDataType(con, .(value))), 1L)
        expect_match(dbDataType(con, .(value)), ".")
      }))
    }

    expect_conn_has_data_type <- function(value) {
      eval(bquote(
        expect_error(check_conn_data_type(.(value)), NA)))
    }

    expect_conn_has_data_type(logical(1))
    expect_conn_has_data_type(integer(1))
    expect_conn_has_data_type(numeric(1))
    expect_conn_has_data_type(character(1))
    expect_conn_has_data_type(Sys.Date())
    expect_conn_has_data_type(Sys.time())
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      expect_conn_has_data_type(blob::as.blob(raw(1)))
    }
  },

  #' }
  NULL
)
