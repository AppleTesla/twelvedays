context("testing sing_day...")

xmas2 <- read.csv("https://www.dropbox.com/s/ap2hqssese1ki4j/xmas_2.csv?dl=1")

# phrased
xmas2 <- xmas2 %>%
  dplyr::mutate(
    Full.Phrase = purrr::pmap_chr(., ~twelvedays::make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
  )

test_that("running sing_day results in a warning when line > 12", {
  expect_warning(twelvedays::sing_day(xmas2, 14, Full.Phrase))
})

test_that("running sing_day ends in two newlines for proper formatting", {
  sung <- twelvedays::sing_day(xmas2, 1, Full.Phrase)
  expect_true(stringr::str_detect(sung, "\n\n$"))
})
