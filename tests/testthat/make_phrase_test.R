context("testing make_phrase...")

xmas2 <- read.csv("https://www.dropbox.com/s/ap2hqssese1ki4j/xmas_2.csv?dl=1")

# phrased
xmas2 <- xmas2 %>%
  dplyr::mutate(
    Full.Phrase = purrr::pmap_chr(., ~twelvedays::make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
  )

test_that("running make_phrase differentiates between A/An", {
  startsWithVowel <- twelvedays::make_phrase(1, "first", "email", NA, NA, "from Cal Poly")
  expect_true(stringr::str_detect(startsWithVowel, "^An"))
})

test_that("running make_phrase differentiates between A/An", {
  noDoubleSpaces <- twelvedays::make_phrase(1, "second", "iPhone", "vibrating", "flashy", "from a friend")
  expect_false(stringr::str_detect(noDoubleSpaces, "[:space:][:space:]"))
})
