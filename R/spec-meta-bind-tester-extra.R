BindTesterExtra <- R6::R6Class(
  "BindTesterExtra",
  portable = TRUE,
  #
  public = list(
  )
)

new_bind_tester_extra <- function(...) {
  R6::R6Class(
    inherit = BindTesterExtra,
    portable = TRUE,
    public = list(...)
  )
}
