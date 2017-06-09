textCorpus <- lineCorpus

ctypemapper <- c(
  line = lineCorpus,
  text = textCorpus,
  pair = pairCorpus
)

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
                       type=config$Corpus$Type) {
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

  if (identical(type, "pair")) {
    meta$ShinyConc.nWordsQ <- stringr::str_count(meta$Q, "\\w+")
    meta$ShinyConc.nWordsA <- stringr::str_count(meta$A, "\\w+")
  } else
    meta$ShinyConc.nWords <- stringr::str_count(meta$text, "\\w+")


  ctypemapper[[type]](meta, KWICcolselect=config$SearchTool$KWIC$DisplayExtraColumns)

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
cacheCorpus <- function(config, corpusdir="corpus", metafile="meta.csv") {
  cachefile <- paste0(corpusdir, "/", "corpus.Rdata")
  metafile <- paste0(corpusdir, "/", metafile)
  meta <- read.csv(metafile, stringsAsFactors = FALSE)
  corpus <- loadCorpus(corpusdir, meta, config)
  mtimes.meta <- file.mtime(metafile)
  mtimes.corpus <- file.mtime(paste0(corpusdir, "/", corpus$corpus$file))
  save(corpus, mtimes.meta, mtimes.corpus, file=cachefile)
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
getCachedCorpus <- function(config, corpusdir="corpus", metafile="meta.csv") {
  cachefile <- paste0(corpusdir, "/", "corpus.Rdata")
  metaf <- paste0(corpusdir, "/", metafile)
    if (file.exists(cachefile)) {
      load(cachefile, envir=.GlobalEnv)
      newmtimes.meta <- file.mtime(metaf)
      newmtimes.corpus <- file.mtime(paste0(corpusdir, "/", corpus$corpus$file))
      if (! (all(newmtimes.corpus <= mtimes.corpus) &&
             newmtimes.meta == mtimes.meta)) {
        cacheCorpus(config, corpusdir, metafile)
        getCachedCorpus(config, corpusdir, metafile)
      }
    } else {
      cacheCorpus(config, corpusdir, metafile)
      getCachedCorpus(config, corpusdir, metafile)
  }
}

