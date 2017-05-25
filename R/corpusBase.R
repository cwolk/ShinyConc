#' Do a KWIC search
#'
#' @param text vector of char
#' @param querystring the querystring
#' @param meta the metadata
#' @param contextlength numer of characters for left/right context
#'
#' @return Data frame with colums: all metadata, left context, match, right context
#' @export
#'
#' @examples
Base.KWIC <- function(text, querystring, meta, contextlength=30) {
  if (querystring %in% c("", "\\b\\b"))
    return(NULL)
  matches <- stringr::str_locate_all(text, querystring)
  if (length(matches) == 0)
    return(data.frame(Results="Nothing found."))
  matchnums <- sapply(matches, nrow)
  cmatch <- rep(text, matchnums)
  mmatches <- do.call(rbind, matches)
  data.frame(meta[rep(1:nrow(meta), matchnums), , drop = FALSE],
             left = stringr::str_sub(cmatch, pmax(mmatches[,"start"] -
                                                contextlength, 1),
                          mmatches[,"start"]-1),
             center = stringr::str_sub(cmatch, mmatches),
             right = stringr::str_sub(cmatch, mmatches[,"end"]+1,
                           pmin(mmatches[,"end"] + contextlength,
                                stringr::str_length(cmatch))),
             stringsAsFactors = FALSE)

}

#' Count corpus matches
#'
#' @param text vector of char
#' @param querystring the querystring
#'
#' @return Data frame with columns: Token, Frequency, Pairs
#' @export
#'
#' @examples
Base.Count <- function(text, querystring) {
  if (querystring == "")
    return(NULL)
  if (isCaseInsensitive(querystring))
    text <- tolower(text)
  tokenlist <- stringr::str_extract_all(text, querystring)
  freqtable <- sort(table(unlist(tokenlist)), decreasing = TRUE)
  pairtable <- table(unlist(lapply(tokenlist, unique)))
  data.frame("Token" = names(freqtable),
             "Frequency" = as.vector(freqtable),
             "Texts" = as.vector(pairtable[names(freqtable)]),
             stringsAsFactors = FALSE)
}
# TODO: make Pairs application-neutral

#' Create a Wordlist
#'
#' @param text a vector of chars
#' @param querystring the querystring
#'
#' @return Data frame with columns: Token, Frequency, Pairs
#' @export
#'
#' @examples
Base.Wordlist <- function(text, querystring) {
  wlquerystring <- stringr::regex("\\w+", ignore_case =
                                  isCaseInsensitive(querystring))
  Base.Count(text, wlquerystring)
}
# TODO: make Pairs application-neutral (only documentation after Base.Count is fixed)


#' Calculate Keywords
#'
#' @param wlist.target Wordlist for target corpus
#' @param wlist.reference Wordlist for reference corpus
#' @param absolute Should words that are rarer in target have negative values?
#'
#' @return Data frame with columns word, target, reference, normalized versions
#' of target and reference, and keyness
#' @export
#'
#' @examples
Base.Keywords <- function(wlist.target, wlist.reference, absolute = TRUE) {

  wlist.target <- wlist.target[,1:2]
  wlist.reference <- wlist.reference[,1:2]

  if(nrow(wlist.target) < 1) stop("Target Corpus Empty")
  if(nrow(wlist.reference) < 1) stop("Reference Corpus Empty")

  #browser()
  tabl <- merge(wlist.target, wlist.reference, all=TRUE, by="Token")
  tabl[is.na(tabl)] <-0
  colnames(tabl) <- c("word", "target", "reference")
  tabl$`target per 100 words` <- round((tabl$target * 100) / sum(tabl$target), 3)
  tabl$`reference per 100 words` <- round((tabl$reference * 100) / sum(tabl$reference), 3)
  tabl$keyness <- round(chisq_value(tabl$target, tabl$reference,
                                    sum(tabl$target), sum(tabl$reference)), 3)
  if (!absolute) {
    tabl$keyness <- tabl$keyness * ifelse(tabl$`target per 100 words` >
                                            tabl$`reference per 100 words`, 1, -1)
  }
  tabl[order(tabl$keyness, decreasing = TRUE),]
}
# TODO: keyness and normalization constant changeable
