spec_driver_class <- list(
  inherits_from_driver = function(ctx) {
    expect_s4_class(ctx$orig_drv, "DBIDriver")
  },

  NULL
)
