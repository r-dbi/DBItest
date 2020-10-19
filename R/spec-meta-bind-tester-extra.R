BindTesterExtra <- R6::R6Class(
  "BindTesterExtra",
  portable = TRUE,
  #
  public = list(
    check_return_value = function(bind_res, res) invisible(NULL),
    patch_bind_values = identity,
    bind_error = function() NA,
    requires_names = function() NA,
    is_repeated = function() FALSE,
    is_premature_clear = function() FALSE,
    is_untouched = function() FALSE
  )
)

new_bind_tester_extra <- function(...) {
  R6::R6Class(
    inherit = BindTesterExtra,
    portable = TRUE,
    public = list(...)
  )
}
