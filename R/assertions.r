not_empty_string <- function(x) {
  assert_that(is.string(x))
  noNA(x) && all(nchar(x) > 0)
}
on_failure(not_empty_string) <- function(call, env) {
  paste0(deparse(call$x), ' is an empty string.')
}

not_null <- function(x) {
  !is.null(x)
}
on_failure(not_null) <- function(call, env) {
  paste0(deparse(call$x), ' is null.')
}

not_empty_character_vector <- function(x) {
  assert_that(is.character(x))
  noNA(x) && all(nchar(x) > 0)
}
on_failure(not_empty_character_vector) <- function(call, env) {
  paste0(deparse(call$x), ' contains empty or missing values')
}