#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' @import english
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location) {

  startsWithVowel <- stringr::str_detect(item, pattern = "^[aeiouAEIOU]")     # Distinguish between use of A or An

  if (num > 1) {
    item <- pluralize_gift(item)
    num <- english::as.english(num)                                           # Use English form of numerical
  } else if (num == 1 && startsWithVowel) {
    num <- "An"
  } else if (num == 1 && !startsWithVowel) {
    num <- "A"
  }

  phrase <- glue::glue("{stringr::str_to_title(num)} {adjective} {item} {verb} {location}",
                       .sep = " ",
                       .na = " ")

  final_phrase <- stringr::str_replace_all(phrase, "[:space:]{2,}", " ")      # Eliminate duplicate spaces
  final_phrase <- stringr::str_trim(final_phrase)                             # Trim whitespace

  return(final_phrase)
}
