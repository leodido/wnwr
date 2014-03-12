context('Shell command syntax')

search_types <- unlist(flatten(search_type))

test_that("Errors related to the 'word' argument", {
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

test_that("Errors related to the 'search' argument", {
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
    structure(paste0(wn_command, " 'word' ", substring(paste(' -', unique(x), collapse = '', sep = ''), 2)), class = c('wn', 'command'))
  )
}

test_that('Supported search types', {
#   # all possible combinations
#   invisible(lapply(seq_len(length(search_types)), function(m) {
#     invisible(combn(
#       search_types,
#       m,
#       test_search_arg,
#       simplify = FALSE
#     ))
#   }))
  # one command, one search type
  invisible(combn(
    search_types,
    1,
    test_search_arg,
    simplify = FALSE
  ))
  # one command, all search types
  invisible(combn(
    search_types,
    length(search_types),
    test_search_arg,
    simplify = FALSE
  ))
  # duplicates removed
  test_search_arg(rep(search_types[3], 3))
})

test_that("errors related to the 'opt' argument", {
  expect_that(wn_cmd('word', search_types[1], NA), throws_error('must be NULL or a character vector'))
  expect_that(wn_cmd('word', search_types[1], 1L), throws_error('must be NULL or a character vector'))
  expect_that(wn_cmd('word', search_types[1], 'x'), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_types[1], c('x')), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_types[1], NA_character_), throws_error('should be one of'))
  expect_that(wn_cmd('word', search_types[1], c('x', 'y')), throws_error('must be of length 1'))
})

test_that("supported search options", {
  expect_equal(wn_cmd('word', search_types[1]), structure(paste0(wn_command, " 'word' -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], NULL), structure(paste0(wn_command, " 'word' -", search_types[1]), class = c('wn', 'command')))
  # all supported options
  invisible(lapply(
    search_opts,
    function(o) expect_equal(wn_cmd('word', search_types[1], o), structure(paste0(wn_command, " 'word' -", o, " -", search_types[1]), class = c('wn', 'command')))
  ))
})

test_that("errors related to the 'sense_num' argument", {
  expect_that(wn_cmd('word', search_types[1], search_opts[1], letters[1:5L]), throws_error('not a count'))
  expect_that(wn_cmd('word', search_types[1], search_opts[1], 'abc'), throws_error('not a count'))
  expect_that(wn_cmd('word', search_types[1], search_opts[1], NA), throws_error('not a count'))
  expect_that(wn_cmd('word', search_types[1], search_opts[1], 1:10L), throws_error('not a count'))
  expect_that(wn_cmd('word', search_types[1], search_opts[1], NA_integer_), throws_error('missing value'))
})

test_that("sense number filter", {
  expect_equal(wn_cmd('word', search_types[1]), structure(paste0(wn_command, " 'word' -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1]), structure(paste0(wn_command, " 'word' -", search_opts[1], " -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], NULL), structure(paste0(wn_command, " 'word' -", search_opts[1], " -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], c()), structure(paste0(wn_command, " 'word' -", search_opts[1], " -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], sense_num = 10L), structure(paste0(wn_command, " 'word' -n", 10L, " -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], sense_num = 1), structure(paste0(wn_command, " 'word' -n", 1, " -", search_types[1]), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], c(1)), structure(paste0(wn_command, " 'word' -", search_opts[1], " -n", 1, " -", search_types[1]), class = c('wn', 'command')))
})

test_that("parameters ignored in word information mode", {
  expect_equal(wn_cmd('word', info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types, info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], letters[1:20], info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], 2L, info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
  expect_equal(wn_cmd('word', search_types[1], search_opts[1], 1:2L, info = TRUE), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
})
