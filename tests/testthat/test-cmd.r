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
  expect_that(wn_cmd('word', NULL), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA), throws_error('not a character vector'))
  expect_that(wn_cmd('word', NA_character_), throws_error('contains empty or missing values'))
  expect_that(wn_cmd('word', ''), throws_error('search contains empty or missing values'))
  expect_that(wn_cmd('w', c('')), throws_error('search contains empty or missing values'))
  expect_that(wn_cmd('w', c('x')), throws_error('should be one of'))
})

test_that('Supported search types', {
  # TODO: iterate all possibile search values and test call result
})

test_that('Function \'opts\' parameter', {
  
})

test_that('Function \'sense_num\' parameter', {
  
})