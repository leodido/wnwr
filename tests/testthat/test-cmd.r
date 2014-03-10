context('Shell command syntax')

test_that("Errors covering 'word' argument", {
  expect_that(wn_cmd(), throws_error('is missing'))
  expect_that(wn_cmd(c()), throws_error('not a string'))
  expect_that(wn_cmd(c('x', 'y')), throws_error('not a string'))
  expect_that(wn_cmd(10), throws_error('not a string'))
  expect_that(wn_cmd(NA_integer_), throws_error('not a string'))
  expect_that(wn_cmd(NULL), throws_error('not a string'))
  expect_that(wn_cmd(NA), throws_error('not a string'))
  expect_that(wn_cmd(NA_character_), throws_error('empty string'))
  expect_that(wn_cmd(c('')), throws_error('empty string'))
  expect_that(wn_cmd(''), throws_error('empty string'))
})

test_that("Errors covering 'search' argument", {
  expect_that(wn_cmd('word'), throws_error('is missing'))
  expect_that(wn_cmd('word', c()), throws_error('not a character vector'))
  expect_that(wn_cmd('word', c()), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NULL), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA_character_), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('word', ''), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('w', c('')), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('w', c('x')), throws_error('should be one of'))
})

test_search_arg <- function(x) {
  expect_equal(
    wn_cmd('word', x),
    paste0("wn 'word' ", substring(paste(' -', x, collapse = '', sep = ''), 2))
  )
}

test_that('Supported search types', {
#   # all possible combinantions
#   invisible(lapply(seq_len(length(search_type)), function(m) {
#     invisible(combn(
#       search_type,
#       m,
#       test_search_arg,
#       simplify = FALSE
#     ))
#   }))
  # one command, one search type
  invisible(combn(
    search_type,
    1,
    test_search_arg,
    simplify = FALSE
  ))
  # one command, all search types
  invisible(combn(
    search_type,
    length(search_type),
    test_search_arg,
    simplify = FALSE
  ))
})

# test_that('Function \'opts\' parameter', {
#   
# })
# 
# test_that('Function \'sense_num\' parameter', {
#   
# })