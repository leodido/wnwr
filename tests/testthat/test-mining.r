context('Text mining')
 
test_that("lexicographer information", {
  lex_1 <- c('<noun.animal> beef, beef cattle', '<noun.animal> cattle, cows, kine, oxen, Bos taurus', '<noun.food> meat1')
  expect_equal(
    extract_lexinfo(lex_1),
    structure(
      list(c('noun.animal', 'noun.animal', 'noun.food'), c('beef, beef cattle', 'cattle, cows, kine, oxen, Bos taurus', 'meat1')),
      .Names = c('lexinfo', 'sense')
    )
  )
  lex_2 <- c('=> <noun.animal> beef, beef cattle', ' => <noun.animal> cattle, cows, kine, oxen, Bos taurus')
  expect_equal(
    extract_lexinfo(lex_2),
    structure(list(c('noun.animal', 'noun.animal'), c('beef, beef cattle', 'cattle, cows, kine, oxen, Bos taurus')), .Names = c('lexinfo', 'sense'))
  )
})

test_that("synset offset", {
  so_1 <- c('{02402425} cattle, cows, kine, oxen, Bos taurus', '{02404186} beef, beef cattle', '{00001740} entity {00001740}')
  expect_equal(
    extract_offset(so_1),
    structure(
      list(c('02402425', '02404186', '00001740'), c('cattle, cows, kine, oxen, Bos taurus', 'beef, beef cattle', 'entity {00001740}')),
      .Names = c('offset', 'sense')
    )
  )
  so_2 <- c('=> {02402425} cattle, cows, kine, oxen, Bos taurus', '=> {02404186} beef, beef cattle')
  expect_equal(
    extract_offset(so_2),
    structure(list(c('02402425', '02404186'), c('cattle, cows, kine, oxen, Bos taurus', 'beef, beef cattle')), .Names = c('offset', 'sense'))
  )
})

test_that("sense number deletion", {
  snd_1 <- 'ciao#1'
  expect_equal(
    delete_sensenum(snd_1),
    'ciao'
  )
  snd_2 <- c('cattle#1')
  expect_equal(
    delete_sensenum(snd_2),
    'cattle'
  )
  snd_3 <- c('cattle#1, cows#1, kine#1, oxen#1, Bos taurus#1', 'bovine#1', 'bovid#1')
  expect_equal(
    delete_sensenum(snd_3),
    c('cattle, cows, kine, oxen, Bos taurus', 'bovine', 'bovid')
  )
})

test_that("gloss and examples", {
  g_1 <- c(
    'gripe, bitch, grouse, crab, beef, squawk, bellyache, holler -- (complain; "What was he hollering about?")',
    'complain, kick, plain, sound off, quetch, kvetch -- (express complaints, discontent, displeasure, or unhappiness; "My mother complains all day"; "She has a lot to kick about")'
  )
  expect_equal(
    extract_gloss(g_1),
    structure(
      list(
        c('complain', 'express complaints, discontent, displeasure, or unhappiness'),
        list('What was he hollering about?', c('My mother complains all day', 'She has a lot to kick about')),
        c('gripe, bitch, grouse, crab, beef, squawk, bellyache, holler', 'complain, kick, plain, sound off, quetch, kvetch')
      ),
      .Names = c('gloss', 'example', 'sense')
    )
  )
})
