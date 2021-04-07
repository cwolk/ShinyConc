#' Returns a querystring
#'
#' @param searchterm what to search for
#' @param searchmode string, word, or regex
#' @param casesensitive whether the search should be casesensitive
#'
#' @return the querystring
#' @export
#'
#' @examples buildQuerystring("actually", "word", FALSE)
buildQuerystring <- function (searchterm, searchmode, casesensitive) {
  #browser()
  out <- switch(searchmode,
            "string" = stringr::coll(searchterm, ignore_case = !casesensitive),
            "word" = stringr::regex(paste0("\\b", quotemeta(searchterm), "\\b"),
                                    ignore_case = !casesensitive),
            "regex" = stringr::regex(searchterm, ignore_case = !casesensitive))
  attributes(out)$searchterm <- searchterm
  attributes(out)$searchmode <- searchmode
  attributes(out)$casesensitive <- casesensitive
  out
}

#' Apply a restriction to a corpus
#'
#' @param corpus The corpus
#' @param restriction The restriction
#' @param querystring An optional querystring - if set, only matching entries will be returned
#'
#' @return the restricted corpus
#' @export
#'
#' @examples
applyRestriction <- function(corpus, restriction, querystring = NULL) {
  evals <- sapply(names(restriction), function(x) {
    if (x %in% colnames(corpus$corpus))
      as.character(corpus$corpus[,x]) %in% restriction[[x]]
    else
      rep(TRUE, nrow(corpus$corpus))
  })
  #browser()
  corpus$corpus <- corpus$corpus[ apply(evals, 1, all),]

  if (!is.null(querystring)) {
    corpus <- filterCorpus(corpus, querystring, restriction)
  }
  corpus
}
