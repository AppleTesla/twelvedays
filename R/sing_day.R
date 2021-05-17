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
#' @import english
#'
#' @export
sing_day <- function(dataset, line, phrase_col) {

  if (line > nrow(dataset)) {                             # Provide a warning
    warning("WARNING: Line argument larger than dataset allows. Using dataset maximum instead.\n\n")
    line <- nrow(dataset)
  }

  phrases <- dataset %>% dplyr::pull({{phrase_col}})

  opening <- glue::glue("On the {dataset[line, ]$Day.in.Words} day of Christmas, my true love sent to me,")

  lines <- phrases[line:1]                                # Order gifts in reverse from last day to first day

  if (line > 1) {                                         # Replace last quantifier with 'And' for grammatical purposes
    lines[line] <- stringr::str_replace(lines[line],
                                  pattern = "^A ",
                                  replacement = "And a ")
    lines[line] <- stringr::str_replace(lines[line],
                                     pattern = "^An ",
                                     replacement = "And an ")
  }

  full_phrase <- paste(lines, collapse = ",\n")           # Concatenate gift lines
  full_phrase <- paste(opening, full_phrase, sep = "\n")  # Concatenate opening line with gift lines
  full_phrase <- paste0(full_phrase, ".")                 # Add a period at the end

  return(full_phrase)
}
