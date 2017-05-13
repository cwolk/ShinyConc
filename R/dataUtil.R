#' Interleave two vectors.
#'
#' @param a the first list
#' @param b the second list
#'
#' @return a vector containing a[1], b[1], a[2], b[2] and so on
#' @export
#'
#' @examples interleave(1:10, 1:10)
interleave <- function(a, b) {
  c(a, b)[order(c(seq_along(a), seq_along(b)))]
}

#' Interleave two data frames
#'
#' @param a the first data frame
#' @param b the second data frame
#'
#' @return a data frame containing a[1,], b[1,], a[2,], b[2,] and so on
#' @export
#'
#' @examples
interleave.df <- function(a,b) {
  rbind(a, b)[interleave(1:nrow(a), nrow(a)+(1:nrow(a))),,drop=FALSE]
}

#' Evaluates whether a querystring is case-insensitive
#'
#' @param querystring the querystring
#'
#' @return a logi indicating whether the querystring is case-insensitive
#' @export
#'
#' @examples
isCaseInsensitive <- function(querystring) {
  if (is.null(attributes(querystring)$options$case_insensitive))
    attributes(querystring)$options$strength < 3
  else
    attributes(querystring)$options$case_insensitive
}
