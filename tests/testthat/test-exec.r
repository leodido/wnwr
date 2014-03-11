context('Command executor')

test_that("errors related to the 'cmd' argument", {
  expect_that(exec("wn 'ciao'"), throws_error('not a wn command'))
  expect_that(exec(NULL), throws_error('not a wn command'))
  expect_that(exec(NA), throws_error('not a wn command'))
  expect_that(exec(NA_character_), throws_error('not a wn command'))
  expect_that(exec(1L), throws_error('not a wn command'))
  expect_that(exec(list()), throws_error('not a wn command'))
  expect_that(exec(), throws_error('arg is missing'))
})

test_that("command execution", {
  files <- file.path('data', list.files('data', pattern = '-exec.rds$'))
  matches <- regexpr('-([^-]+)-', files)
  matches <- matches + 1
  attr(matches, 'match.length') <- attr(matches, 'match.length') - 2
  words <- regmatches(files, matches)
  invisible(sapply(seq_along(files), function(i) {
    expect_equal(
      exec(structure(paste0("wn '", words[[i]], "'"), class = c('wn', 'command'))),
      readRDS(files[[i]])
    )
  }))
})