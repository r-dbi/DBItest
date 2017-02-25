BindTesterExtra <- R6::R6Class(
  "BindTesterExtra",
  portable = TRUE,

  public = list(
    check_return_value = function(bind_res, res) invisible(NULL),
    patch_bind_values = identity,
    requires_names = function() FALSE,
    is_repeated = function() FALSE
  )
)

new_extra <- function(...) {
  R6::R6Class(
    inherit = BindTesterExtra,
    portable = TRUE,
    public = list(...)
  )
}
