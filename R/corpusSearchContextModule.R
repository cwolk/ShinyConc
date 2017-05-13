#' Title
#'
#' @param id
#' @param config
#'
#' @return
#' @export
#'
#' @examples
searchContextOutput <- function(id, config) {
  ns <- NS(id)
  if (identical(config$ContextDisplay$Type, "Multicolumns")) {
    fluidRow(
      lapply(names(config$ContextDisplay$Columns), function(columnName) {
        column(12/length(config$ContextDisplay$Columns),
               h4(columnName),
               uiOutput(ns(config$ContextDisplay$Columns[columnName])))
    }))
  }
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param config
#' @param searchTool
#' @param mainCorpus
#'
#' @return
#' @export
#'
#' @examples
searchContextModule <- function(input, output, session, config,
                                      searchTool, mainCorpus) {

  if (identical(config$ContextDisplay$Type, "Multicolumns")) {
    lapply(config$ContextDisplay$Columns, function(column) {
      output[[column]] <- renderUI(
        if (! is.null(searchTool$selected())) {
#          browser()
          queryS <- mainCorpus$query$querystring()
          if (queryS == "")
            p(searchTool$selected()[[column]])
          else
            HTML(annotate_html(searchTool$selected()[[column]], queryS))

        } else p("select concordance line"))
    })
  }

}
