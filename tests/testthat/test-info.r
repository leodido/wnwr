context('Word informations')

synset_types <- unlist(unname(synset_type))
search_types <- unlist(flatten(search_type))

test_that("errors related to the 'info' argument", {
  expect_that(extract_info(), throws_error('arg is missing'))
  expect_that(extract_info(NA), throws_error('not a wn result'))
  expect_that(extract_info(list()), throws_error('not a wn result'))
  expect_that(extract_info(c('')), throws_error('not a wn result'))
  expect_that(extract_info('single information'), throws_error('not a wn result'))
  expect_that(extract_info(NA_character_), throws_error('not a wn result'))
})

test_that("information extraction", {
  expect_null(extract_info(NULL))
  expect_null(extract_info(c()))
  # no information detected
  expect_equal(
    extract_info(structure(list(synset_types, 1), .Names = c('result', 'num_senses'), class = c('wn', 'result'))),
    structure(vector('list', length(synset_types)), .Names = synset_types)
  )
  # some common cases
  obj_1 <- structure(
    list(
      c(synset_types[1], search_types[1], synset_types[2], search_types[2], synset_types[3], search_types[3], synset_types[4], search_types[4]),
      1
    ),
    .Names = c('result', 'num_senses'),
    class = c('wn', 'result')
  )
  expect_equal(
    extract_info(obj_1),
    structure(list(search_types[1], search_types[2], search_types[3], search_types[4]), .Names = synset_types)
  )
  obj_2 <- structure(
    list(
      c(synset_types[1], search_types[10], synset_types[4], search_types[1:9], synset_types[3], search_types[11], synset_types[2]),
      1
    ),
    .Names = c('result', 'num_senses'),
    class = c('wn', 'result')
  )
  expect_equal(
    extract_info(obj_2),
    structure(list(search_types[10], NULL, search_types[11], search_types[1:9]), .Names = synset_types)
  )
  # some real cases
  files <- file.path('data', list.files('data', pattern = '-info.rds$'))
  matches <- regexpr('-([^-]+)-', files)
  matches <- matches + 1
  attr(matches, 'match.length') <- attr(matches, 'match.length') - 2
  words <- regmatches(files, matches)
  invisible(sapply(seq_along(files), function(i) {
    expect_equal(
      info(words[[i]]),
      readRDS(files[[i]])
    )
  }))
})

test_that("errors related to the 'word' argument", {
  expect_that(info(), throws_error('arg is missing'))
  expect_that(info(''), throws_error('an empty string'))
  expect_that(info(NULL), throws_error('not a string'))
  expect_that(info(NA), throws_error('not a string'))
})

test_that("word information command syntax", {
  expect_equal(info_cmd('word'), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
})

test_that("errors related to the 'synset' argument", {
  expect_that(has('ciao', 'hypen', NA), throws_error('not a character vector'))
  expect_that(has('ciao', 'hypen', NA, details = TRUE), throws_error('not a character vector'))
  expect_that(has('ciao', 'hypen', list()), throws_error('not a character vector'))
  expect_that(has('ciao', 'hypen', c('')), throws_error('empty or missing values'))
  expect_that(has('ciao', 'hypen', NA_character_), throws_error('empty or missing values'))
})

test_that("has information", {
  expect_true(has('ciao', 'hypen', synset = NULL))
  expect_equal(
    has('ciao', 'hypen', synset = NULL, details = TRUE),
    structure(c(TRUE, FALSE, FALSE, FALSE), .Names = c("noun", "verb", "adj", "adv"))
  )
  expect_true(has('ciao', 'hypen', c()))
  expect_equal(
    has('ciao', 'hypen', c(), details = TRUE),
    structure(c(TRUE, FALSE, FALSE, FALSE), .Names = c("noun", "verb", "adj", "adv"))
  )
})
