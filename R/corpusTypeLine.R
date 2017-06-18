#' Initialize lineCorpus
#'
#' @param corpusdf
#' @param KWICcolselect
#'
#' @return
#' @export
#'
#' @examples
lineCorpus <- function(corpusdf, KWICcolselect = "ID") {
  corp <- basicCorpus(corpusdf, KWICcolselect)
  class(corp) <- append(class(corp),"lineCorpus")
  corp
}

#' filter a lineCorpus
#'
#' @param corpus
#' @param querystring
#' @param controls
#' @param ... additional parameters
#'
#' @return filtered corpus
#' @export
#'
#' @examples
filterCorpus.lineCorpus <- function(corpus, querystring, controls, ...) {
  if (attributes(querystring)$searchterm == "")
    return(corpus)
  corpus$corpus <- corpus$corpus[str_detect(corpus$corpus$text,
                                            querystring),]
  corpus
}

#' Create KWIC for lineCorpus
#'
#' @param corpus
#' @param querystring
#' @param controls
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
getKWIC.lineCorpus <- function(corpus, querystring, controls, ...) {
  obj <- corpus
  corpus <- corpus$corpus
  Base.KWIC(corpus[,"text"], querystring, corpus)
}

#' Count occurrences for lineCorpus
#'
#' @param corpus
#' @param querystring
#' @param controls
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
getCounts.lineCorpus <- function(corpus, querystring, controls, ...) {
  obj <- corpus
  corpus <- corpus$corpus
  Base.Count(corpus[,"text"], querystring)
}

#' Create Wordlist for lineCorpus
#'
#' @param corpus corpus
#' @param querystring querystring
#' @param controls mode
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
getWordlist.lineCorpus <- function(corpus, querystring, controls, ...) {
  obj <- corpus
  corpus <- filterCorpus(corpus, querystring, controls, ...)$corpus
  Base.Wordlist(corpus[,"text"], querystring)
}
