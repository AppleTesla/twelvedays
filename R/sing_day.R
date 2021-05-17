#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, phrase_col){

  # phrases <- dataset %>% dplyr::pull({{phrase_col}})

  opening <- glue::glue("On the {dataset[line, ]$Day.in.Words}")

  return("opening")

  # total_phrase <- purrr::pmap_chr(dataset, make_phrase(dataset$Day))
}
