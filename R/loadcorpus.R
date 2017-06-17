textCorpus <- lineCorpus

ctypemapper <- c(
  line = lineCorpus,
  text = textCorpus,
  pair = pairCorpus
)

process_addNwords <- function(corpus, type){
  if (identical(type, "pair")) {
    corpus$ShinyConc.nWordsQ <- stringr::str_count(corpus$Q, "\\w+")
    corpus$ShinyConc.nWordsA <- stringr::str_count(corpus$A, "\\w+")
  } else
    corpus$ShinyConc.nWords <- stringr::str_count(corpus$text, "\\w+")
  corpus
}

processCorpus <- function(corpus, config) {
  corpus <- process_addNwords(corpus, config$Corpus$Type)
}


#' Load corpus
#'
#' @param corpusdir
#' @param meta
#' @param source
#' @param type
#' @param config
#'
#' @return
#' @export
#'
#' @examples
loadCorpus <- function(corpusdir, meta, config=NULL,
                       charset=config$corpusCharset,
                       source=config$Corpus$Source,
                       type=config$Corpus$Type, ...) {
  if (identical(source, "file")) {
    if ("text" %in% colnames(meta))
      stop("text column already present in the metadata")
    if (! "file" %in% colnames(meta))
      stop("there is no file column in the metadata")
    meta$text <- sapply(meta$file, function(x){
      enco <- if (is.null(config$corpusCharset) ||
                  identical(config$corpusCharset, "Auto"))
        readr::guess_encoding(paste0(corpusdir, "/", x))$encoding[1] else
          config$corpusCharset

      text <- tryCatch(readr::read_file(paste0(corpusdir, "/", x),
                                        locale= readr::locale(encoding= enco)),
                       error = function(e) {
                         readr::read_file(paste0(corpusdir, "/", x))
                       })
      cat("File ", x, " loaded\n")
      gsub("\\s+", " ", text)
      })
  }

  meta <- processCorpus(meta, config)

  corpus <- ctypemapper[[type]](meta, KWICcolselect=config$SearchTool$KWIC$DisplayExtraColumns)

  return(function(...) corpus)

}


#' Title
#'
#' @param config
#' @param corpusdir
#' @param metafile
#'
#' @return
#' @export
#'
#' @examples
cacheCorpus <- function(config, corpusdir="corpus", metafile="meta.csv",
                        corpusloader=loadCorpus, filelist=NULL, ...) {
  # TODO: branch if custom, don't read meta
  cachefile <- paste0(corpusdir, "/", "corpus.Rdata")
  if (config$Corpus$Source != "custom") {
    metafile <- paste0(corpusdir, "/", metafile)
    meta <- read.csv(metafile, stringsAsFactors = FALSE)
  } else if (is.null(filelist))
    stop("Please supply a file list to use caching with custom corpus readers.")
  if (is.null(filelist))
    filelist <- c(metafile, if(config$Corpus$Source == "file") paste0(corpusdir, "/", meta$file) else NULL)
  mtimes <- file.mtime(filelist)
  selectCorpus <- corpusloader(corpusdir, meta, config, ...)
  attributes(selectCorpus)$filelist <-  filelist
  attributes(selectCorpus)$mtimes   <-  mtimes
  save(selectCorpus, file=cachefile)
}

#' Title
#'
#' @param config
#' @param corpusdir
#' @param metafile
#'
#' @return
#' @export
#'
#' @examples
getCachedCorpus <- function(config, corpusdir="corpus", metafile="meta.csv",
                            filelist = NULL, corpusloader=loadCorpus) {
  cachefile <- paste0(corpusdir, "/", "corpus.Rdata")
  orig_filelist <- filelist
    if (file.exists(cachefile)) {
      load(cachefile, envir=.GlobalEnv)
      if (is.null(filelist))
        filelist <- c(attributes(selectCorpus)$filelist)
      mtimes <- file.mtime(filelist)
      if (! ((filelist == attributes(selectCorpus)$filelist) &&
             all(mtimes <= attributes(selectCorpus)$mtimes))) {
        cacheCorpus(config, corpusdir, metafile, corpusloader=corpusloader,
                    filelist=orig_filelist)
        getCachedCorpus(config, corpusdir, metafile, corpusloader=corpusloader,
                        filelist=orig_filelist)
      }
    } else {
      cacheCorpus(config, corpusdir, metafile, corpusloader=corpusloader,
                  filelist=orig_filelist)
      getCachedCorpus(config, corpusdir, metafile, corpusloader=corpusloader,
                      filelist=orig_filelist)
  }
}

