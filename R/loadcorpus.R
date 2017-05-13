textCorpus <- lineCorpus

ctypemapper <- c(
  line = lineCorpus,
  text = textCorpus,
  pair = pairCorpus
)

#' Load corpus
#'
#' @param dir
#' @param meta
#' @param source
#' @param type
#' @param config
#'
#' @return
#' @export
#'
#' @examples
loadCorpus <- function(dir, meta, source, type, config, corpusdir="corpus") {
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

  ctypemapper[[type]](meta, KWICcolselect=config$SearchTool$KWIC$DisplayExtraColumns)

}
