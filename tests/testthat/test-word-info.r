context('Word informations')

test_that("errors related to the 'word' argument", {
  expect_that(word_info_cmd(), throws_error('arg is missing'))
  expect_that(word_info_cmd(''), throws_error('an empty string'))
  expect_that(word_info_cmd(NULL), throws_error('not a string'))
  expect_that(word_info_cmd(NA), throws_error('not a string'))
})

test_that('word information command', {
  expect_equal(word_info_cmd('word'), paste0(wn_command, " 'word'"))
})