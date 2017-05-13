#' Generic method for filtering a corpus object
#'
#' @param corpus The corpus to filter
#' @param querystring The querystring to use for filtering
#' @param controls
#' @param ... additional parameters for implementation
#'
#' @return a filtered corpus
#' @export
#'
# @examples filterCorpus(corpus, querystring)
filterCorpus <- function(corpus, querystring, controls, ...)
  UseMethod("filterCorpus")


#' Generic for creating a KWIC view
#'
#' @param corpus The corpus to search
#' @param querystring The querystring to use for searching
#' @param controls
#' @param ... additional parameters for implementation
#'
#' @return KWIC view; see Base.KWIC
#' @export
#'
# @examples getKWIC(corpus, querystring)
getKWIC <- function(corpus, querystring, controls, ...)
  UseMethod("getKWIC")


#' Generic for deriving corpus counts
#'
#' @param corpus The corpus to search
#' @param querystring The querystring to use for searching
#' @param controls
#' @param ... additional parameters for implementation
#'
#' @return corpus counts; see Base.count
#' @export
#'
# @examples getCounts(corpus, querystring)
getCounts <- function(corpus, querystring, controls, ...)
  UseMethod("getCounts")


#' Create Wordlists
#'
#' @param corpus The corpus to search
#' @param querystring The querystring to use for searching
#' @param controls
#' @param ... additional parameters for implementation
#'
#' @return a Wordlist; see Base.Wordlist
#' @export
#'
# @examples getWordlist(corpus, querystring)
getWordlist <- function(corpus, querystring, controls, ...)
  UseMethod("getWordlist")
