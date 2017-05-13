#' Function that wraps 'match' and 'nomatch' spans around the parts of the string input that do or do not match the pattern input.
#'
#' @param string the string to annotate
#' @param pattern the regular expression to test
#'
#' @return a string with span tags around matching/non-matching sequences
#' @export
#'
#' @examples
annotate_html <- function(string, pattern) {
  locs <- stringr::str_locate_all(string, pattern)[[1]]
  ## TODO FIXME ugglly workaround for multiple qs with same id
  if(length(string) > 1) string <- string[1]
  current_pos <- 1
  result <- ""
  if (nrow(locs) < 1)
    return(string)
  for (i in 1:nrow(locs)) {
    #if(is.na(locs[i, 1] > current_pos)) browser()
    if (locs[i, 1] > current_pos)
      result <- stringr::str_c(result, toString(span(stringr::str_sub(
        string, current_pos, locs[i,1]-1), type="nomatch")))
    result <- stringr::str_c(result, toString(span(stringr::str_sub(
      string, locs[i,1], locs[i, 2]), type="match")))
    current_pos <- locs[i, 2] + 1
  }
  if (current_pos < str_length(string))
    result <- stringr::str_c(result, toString(span(stringr::str_sub(
      string, current_pos, stringr::str_length(string)), type="nomatch")))
  result
}

#' Helper function for preparing datateble output columnDefs
#'
#' @param data the data to be desplayed in the table
#' @param map a list with desired output classes as names and the corresponding column names as values
#'
#' @return a map of classes
#' @export
#'
#' @examples
createClassesMap <- function(data, map) {
  lapply(map, function(x) list(className=x[1],
                               targets=which(colnames(data) == x[2]) - 1))
}
