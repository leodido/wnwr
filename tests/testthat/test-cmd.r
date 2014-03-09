context('Shell command syntax')

test_that('wn_cmd errors', {
  expect_that(wn_cmd('word'), throws_error())
})