#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift) {
  oo_regex <- "oo"
  y_regex <- "y$"

  oo_pattern <- stringr::str_detect(gift, pattern = oo_regex) == TRUE
  y_pattern <- stringr::str_detect(gift, pattern = y_regex) == TRUE
  neither_pattern <- !(y_pattern|oo_pattern)

  gift[oo_pattern] <- stringr::str_replace(gift[oo_pattern],
                                   pattern = oo_regex,
                                   replacement = "ee")

  gift[y_pattern] <- stringr::str_replace(gift[y_pattern],
                                  pattern = y_regex,
                                  replacement = "ies")

  gift[neither_pattern] <- stringr::str_replace(gift[neither_pattern],
                                        pattern = "$",
                                        replacement = "s")

  return(gift)
}
