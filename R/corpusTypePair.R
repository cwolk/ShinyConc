#' Initialize pairCorpus
#'
#' @param corpusdf
#' @param KWICcolselect
#'
#' @return
#' @export
#'
#' @examples
pairCorpus <- function(corpusdf, KWICcolselect = "ID") {
  corp <- basicCorpus(corpusdf, KWICcolselect, type="pair")
  class(corp) <- append(class(corp),"pairCorpus")
  corp
}

#' filter a pairCorpus
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
filterCorpus.pairCorpus <- function(corpus, querystring, controls, ...) {
  if (attributes(querystring)$searchterm == "")
    return(corpus)
  corpus$corpus <- switch(controls$ShinyConc.mode,
         "Q"= corpus$corpus[str_detect(corpus$corpus$Q, querystring),],
         "A"= corpus$corpus[str_detect(corpus$corpus$A, querystring),],
         "Q|A" = corpus$corpus[str_detect(corpus$corpus$Q, querystring) |
           str_detect(corpus$corpus$A, querystring),],
         "Q&A"= corpus$corpus[str_detect(corpus$corpus$Q, querystring) &
           str_detect(corpus$corpus$A, querystring),])
  corpus
}

#' Create KWIC for pairCorpus
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
getKWIC.pairCorpus <- function(corpus, querystring, controls, clength, ...) {
  obj <- corpus
  corpus <- filterCorpus(corpus, querystring, controls)$corpus
  if (controls$ShinyConc.mode %in% c("Q|A", "Q&A")) {
    meta <- interleave.df(corpus, corpus)
    Base.KWIC(cbind(corpus$Q, corpus$A), querystring, meta, clength)
  } else {
    Base.KWIC(corpus[,controls$ShinyConc.mode], querystring,
              corpus, clength)
  }
}

#' Count occurrences for pairCorpus
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
getCounts.pairCorpus <- function(corpus, querystring, controls, ...) {
  obj <- corpus
  corpus <- corpus$corpus
  if (controls$ShinyConc.mode %in% c("Q|A", "Q&A"))
    Base.Count(paste(corpus$Q, corpus$A), querystring)
  else
    Base.Count(corpus[,controls$ShinyConc.mode], querystring)
}

#' Create Wordlist for pairCorpus
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
getWordlist.pairCorpus <- function(corpus, querystring, controls, ...) {
  obj <- corpus
  corpus <- filterCorpus(corpus, querystring, controls, ...)$corpus
  if (controls$ShinyConc.mode %in% c("Q|A", "Q&A"))
    Base.Wordlist(paste(corpus$Q, corpus$A), querystring)
  else
    Base.Wordlist(corpus[,controls$ShinyConc.mode], querystring)
}

#' Title
#'
#' @param corpus
#' @param controls
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getWordcount.pairCorpus <- function(corpus, controls, ...){
  switch(controls$ShinyConc.mode,
         "Q"= sum(corpus$corpus$ShinyConc.nWordsQ),
         "A"= sum(corpus$corpus$ShinyConc.nWordsA),
         "Q|A" = sum(corpus$corpus$ShinyConc.nWordsQ) +
                 sum(corpus$corpus$ShinyConc.nWordsA),
         "Q&A"= sum(corpus$corpus$ShinyConc.nWordsQ) +
                sum(corpus$corpus$ShinyConc.nWordsA))
}
