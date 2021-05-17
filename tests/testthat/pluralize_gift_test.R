xmas2 <- read.csv("https://www.dropbox.com/s/ap2hqssese1ki4j/xmas_2.csv?dl=1")

# phrased
xmas2 <- xmas2 %>%
  dplyr::mutate(
    Full.Phrase = purrr::pmap_chr(., ~twelvedays::make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
  )

test_that("running puralize_gift identifies ee pattern", {
  goose <- "goose"
  expect_equal(twelvedays::pluralize_gift(goose), "geese")
})

test_that("running puralize_gift identifies y pattern", {
  y <- "ferry"
  expect_equal(twelvedays::pluralize_gift(y), "ferries")
})
