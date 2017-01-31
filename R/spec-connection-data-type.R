spec_connection_data_type <- list(
  data_type_connection = function(ctx) {
    con <- connect(ctx)
    on.exit(dbDisconnect(con), add = TRUE)
    test_data_type(ctx, con)
  },

  NULL
)
