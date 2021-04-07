
#' Escape regular expression syntax in a string. Copied from http://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
#'
#' @param string The string to escape
#'
#' @return the original string ready to be used as a regular expression
#' @export
#'
#' @examples
quotemeta <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}

#' Compute Chi squared Keyness
#'
#' @param a Counts in target corpus
#' @param b Counts in reference corpus
#' @param ta Number of words in target corpus
#' @param tb Number of words in reference corpus
#'
#' @return Chi squared keyness
#' @export
#'
#' @examples
chisq_value <- function(a,b,ta,tb) {
  # TODO: Decide whether implementing correction makes sense
  expected_a <- (a+b) * (ta / (ta+tb))
  expected_b <- (a+b) * (tb / (ta+tb))
  expected_nota <- (ta - a + tb -b) * (ta / (ta + tb))
  expected_notb <- (ta - a + tb -b) * (tb / (ta + tb))
  rowSums(cbind(((a-expected_a)^2)/expected_a, ((b-expected_b)^2)/expected_b,
                (((ta-a)-expected_nota)^2)/expected_nota, (((tb-b)-expected_notb)^2)/ expected_notb))
}

#' Compute LogLik Keyness
#'
#' @param a Counts in target corpus
#' @param b Counts in reference corpus
#' @param ta Number of words in target corpus
#' @param tb Number of words in reference corpus
#'
#' @return LogLik keyness
#' @export
#'
#' @examples
loglik_value <- function(a,b,ta,tb) {
  expected_a <- (a+b) * (ta / (ta+tb))
  expected_b <- (a+b) * (tb / (ta+tb))
  nota <- ta-a
  notb <- tb-b
  expected_nota <- (nota + notb) * (ta / (ta + tb))
  expected_notb <- (nota + notb) * (tb / (ta + tb))
  2*rowSums(cbind(ifelse(a>0, a * log(a/expected_a), 0),
                  ifelse(b>0, b * log(b/expected_b), 0),
                  ifelse(nota > 0, nota * log(nota/expected_nota), 0),
                  ifelse(notb > 0, notb * log(notb/expected_notb), 0)))

}
