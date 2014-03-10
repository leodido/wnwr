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
  expect_that(wn_cmd('word', NA_integer_), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NULL), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA_character_), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('word', ''), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('word', c('')), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('word', c('x')), throws_error('should be one of'))
})

test_search_arg <- function(x) {
  expect_equal(
    wn_cmd('word', x),
    paste0(wn_command, " 'word' ", substring(paste(' -', unique(x), collapse = '', sep = ''), 2))
  )
}

test_that('Supported search types', {
#   # all possible combinations
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
  # duplicates removed
  test_search_arg(rep(search_type[3], 3))
})

test_that("Errors covering 'opt' argument", {
  expect_that(wn_cmd('word', search_type[1], NA), throws_error('must be NULL or a character vector'))
  expect_that(wn_cmd('word', search_type[1], 1L), throws_error('must be NULL or a character vector'))
  expect_that(wn_cmd('word', search_type[1], 'x'), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_type[1], c('x')), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_type[1], NA_character_), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_type[1], c('x', 'y')), throws_error('must be of length 1'))
})

test_that("Supported search options", {
  expect_equal(wn_cmd('word', search_type[1]), paste0(wn_command, " 'word' -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], NULL), paste0(wn_command, " 'word' -", search_type[1]))
  # all supported options
  invisible(lapply(
    search_opts,
    function(o) expect_equal(wn_cmd('word', search_type[1], o), paste0(wn_command, " 'word' -", o, " -", search_type[1]))
  ))
})

test_that("Errors covering 'sense_num' argument", {
  expect_that(wn_cmd('word', search_type[1], search_opts[1], letters[1:5L]), throws_error('not a count'))
  expect_that(wn_cmd('word', search_type[1], search_opts[1], 'abc'), throws_error('not a count'))
  expect_that(wn_cmd('word', search_type[1], search_opts[1], NA), throws_error('not a count'))
  expect_that(wn_cmd('word', search_type[1], search_opts[1], 1:10L), throws_error('not a count'))
  expect_that(wn_cmd('word', search_type[1], search_opts[1], NA_integer_), throws_error('missing value'))
})

test_that("Sense number filter", {
  expect_equal(wn_cmd('word', search_type[1]), paste0(wn_command, " 'word' -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], search_opts[1]), paste0(wn_command, " 'word' -", search_opts[1], " -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], search_opts[1], NULL), paste0(wn_command, " 'word' -", search_opts[1], " -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], search_opts[1], c()), paste0(wn_command, " 'word' -", search_opts[1], " -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], sense_num = 10L), paste0(wn_command, " 'word' -n", 10L, " -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], sense_num = 1), paste0(wn_command, " 'word' -n", 1, " -", search_type[1]))
  expect_equal(wn_cmd('word', search_type[1], search_opts[1], c(1)), paste0(wn_command, " 'word' -", search_opts[1], " -n", 1, " -", search_type[1]))
})
