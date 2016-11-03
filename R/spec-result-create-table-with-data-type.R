#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{Create table with data type}{
spec_result_create_table_with_data_type <- list(
  #' SQL Data types exist for all basic R data types, and the engine can
  #' process them.
  data_type_connection = function(ctx) {
    with_connection({
      check_connection_data_type <- function(value) {
        eval(bquote({
          expect_is(dbDataType(con, .(value)), "character")
          expect_equal(length(dbDataType(con, .(value))), 1L)
          expect_error({
            as_is_type <- dbDataType(con, I(.(value)))
            expect_identical(dbDataType(con, .(value)), as_is_type)
          }
          , NA)
          expect_error({
            unknown_type <- dbDataType(con, structure(.(value),
                                                      class = "unknown1"))
            expect_identical(dbDataType(con, unclass(.(value))), unknown_type)
          }
          , NA)
          query <- paste0("CREATE TABLE test (a ", dbDataType(con, .(value)),
                          ")")
        }))

        eval(bquote({
          expect_error(dbExecute(con, .(query)), NA)
          on.exit(expect_error(dbExecute(con, "DROP TABLE test"), NA),
                  add = TRUE)
        }))
      }

      expect_conn_has_data_type <- function(value) {
        eval(bquote(
          expect_error(check_connection_data_type(.(value)), NA)))
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
    })
  },

  #' SQL data type for factor is the same as for character.
  data_type_factor = function(ctx) {
    with_connection({
      expect_identical(dbDataType(con, letters),
                       dbDataType(con, factor(letters)))
      expect_identical(dbDataType(con, letters),
                       dbDataType(con, ordered(letters)))
    })
  },

  #' }
  NULL
)
