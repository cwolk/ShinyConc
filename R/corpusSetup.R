process_addID <- function(corpus) {
  if ("ShinyConc.ID" %in% colnames(corpus))
    return(corpus)
  corpus$ShinyConc.ID <- 1:nrow(corpus)
  corpus
}

process_addNwords <- function(corpus, type){
  if (identical(type, "pair")) {
    if (all(c("ShinyConc.nWordsQ", "ShinyConc.nWordsQ")  %in% colnames(corpus)))
      return(corpus)
    corpus$ShinyConc.nWordsQ <- stringr::str_count(corpus$Q, "\\w+")
    corpus$ShinyConc.nWordsA <- stringr::str_count(corpus$A, "\\w+")
  } else
    if ("ShinyConc.nWords" %in% colnames(corpus))
      return(corpus)
    corpus$ShinyConc.nWords <- stringr::str_count(corpus$text, "\\w+")
  corpus
}

processCorpus <- function(corpus, config) {
  corpus <- process_addNwords(corpus, config$Corpus$Type)
  corpus <- process_addID(corpus)
}




#' Basic corpus intialization
#'
#' @param corpusdf
#' @param KWICcolselect
#'
#' @return
#' @export
#'
#' @examples
basicCorpus <- function(corpusdf, KWICcolselect = "ID") {
  corp <- structure(list(), class="basicCorpus")
  corp$corpus <- corpusdf
  corp$KWICcolselect <- KWICcolselect
  corp
}
