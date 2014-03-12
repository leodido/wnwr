context('Word informations')

synset_types <- unlist(unname(synset_type))
search_types <- unlist(flatten(search_type))

test_that("errors related to the 'info' argument", {
  expect_that(process_info(), throws_error('arg is missing'))
  expect_that(process_info(NA), throws_error('not a character vector'))
  expect_that(process_info(list()), throws_error('not a character vector'))
  expect_that(process_info(c('')), throws_error('is a string'))
  expect_that(process_info('single information'), throws_error('is a string'))
  expect_that(process_info(NA_character_), throws_error('is a string'))
})

test_that("information extraction", {
  expect_null(process_info(NULL))
  expect_null(process_info(c()))
  # no information detected
  expect_equal(
    process_info(synset_types),
    structure(vector('list', length(synset_types)), .Names = synset_types)
  )
  # some common cases
  expect_equal(
    process_info(c(synset_types[1], search_types[1], synset_types[2], search_types[2], synset_types[3], search_types[3], synset_types[4], search_types[4])),
    structure(list(search_types[1], search_types[2], search_types[3], search_types[4]), .Names = synset_types)
  )
  expect_equal(
    process_info(c(synset_types[1], search_types[10], synset_types[4], search_types[1:9], synset_types[3], search_types[11], synset_types[2])),
    structure(list(search_types[10], NULL, search_types[11], search_types[1:9]), .Names = synset_types)
  )
  # some real cases
  output <- file.path('data', list.files('data', pattern = '-info.txt$'))
  wanted <- lapply(paste0(tools::file_path_sans_ext(output), '.rds'), readRDS)
  invisible(sapply(seq_along(output), function(i) {
    expect_equal(
      process_info(readLines(output[[i]])),
      wanted[[i]]
    )
  }))
})

test_that("errors related to the 'word' argument", {
  expect_that(info(), throws_error('arg is missing'))
  expect_that(info(''), throws_error('an empty string'))
  expect_that(info(NULL), throws_error('not a string'))
  expect_that(info(NA), throws_error('not a string'))
})

test_that('word information command syntax', {
  expect_equal(word_info_cmd('word'), structure(paste0(wn_command, " 'word'"), class = c('wn', 'command')))
})
