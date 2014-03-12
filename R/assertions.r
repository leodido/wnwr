not_empty_string <- function(x) {
  assert_that(is.string(x))
  noNA(x) && all(nchar(x) > 0)
}
on_failure(not_empty_string) <- function(call, env) {
  paste0(deparse(call$x), ' is an empty string.')
}

not_null <- function(x) !is.null(x)
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

# not_string <- function(x) !is.string(x)
# on_failure(not_string) <- function(call, env) {
#   paste0(deparse(call$x), ' is a string.')
# }

has_class <- function(x, which) has_attr(x, 'class') && which %in% attr(x, 'class', exact = TRUE)
on_failure(has_class) <- function(call, env) {
  paste0(deparse(call$x), " does not have class '", eval(call$which, env), "'.")
}

is_command <- function(x) is.string(x) && has_class(x, 'wn') && has_class(x, 'command')
on_failure(is_command) <- function(call, env) {
  paste0(deparse(call$x), " is not a wn command.")
}

is_result <- function(x) is.list(x) && x %has_name% 'result' && x %has_name% 'num_senses' && has_class(x, 'wn') && has_class(x, 'result')
on_failure(is_result) <- function(call, env) {
  paste0(deparse(call$x), " is not a wn result.")
}