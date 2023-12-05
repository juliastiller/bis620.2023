#' Query keywords from a database table.
#'
#' @details
#' This function performs a keyword search in a specified column of a database table (`d`).
#' It allows for flexible matching options, including case-insensitive search and the choice
#' between searching for values that match all of the keywords (intersection) or any of the keywords (union).
#'
#' @param d A database table.
#' @param kwds The keywords to look for in the specified column.
#' @param column The column to look for the keywords in.
#' @param ignore_case Should the case be ignored when searching for a keyword? (default: TRUE)
#' @param match_all Should the function look for values that match all of the keywords (intersection) or any of the keywords (union)? (default: FALSE; union).
#'
#' @return A filtered version of the input database table based on the keyword search.
#'
#' @importFrom dplyr filter
#'
#' @export
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}
