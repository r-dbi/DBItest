BindTesterExtra <- R6::R6Class(
  "BindTesterExtra",
  portable = TRUE,
  #
  public = list(
    check_return_value = function(bind_res, res) invisible(NULL),
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
