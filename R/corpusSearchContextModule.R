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
  } else if (identical(config$ContextDisplay$Type, "LocalContext")) {
    tagList(
      bsModal("fulltextmodal", title="Full Text", size="large",
              trigger=ns("fulltextbutton"), uiOutput(ns("fulltextView"))),
      fluidRow(column(4, h4("Context")),
               column(4, NULL),
               column(4, actionButton(ns("fulltextbutton"),
                                      label="Show full text"))),
      uiOutput(ns("localcontext"))
    )
  } else if (identical(config$ContextDisplay$Type, "LineContext"))
    htmlOutput(ns("contextview"))

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
          if (attributes(mainCorpus$query$querystring())$searchterm == "")
            p(searchTool$selected()[[column]])
          else
            HTML(annotate_html(searchTool$selected()[[column]],
                               mainCorpus$query$querystring()))

        } else p("select concordance line"))
    })
  } else if (identical(config$ContextDisplay$Type, "LocalContext")) {
    output$localcontext<- renderUI({
      if (! is.null(searchTool$selected())) {
        if ( searchTool$mode() == "Data")
          positions <- cbind(1, 1000)
        else
          positions <- cbind(
            max(1, searchTool$selected()$ShinyConc.KWICmatchStart - 700),
            min(
              stringr::str_length(searchTool$selected()$text),
              searchTool$selected()$ShinyConc.KWICmatchEnd + 700))
        extract <- stringr::str_sub(searchTool$selected()$text, positions)
        if (is.na(extract)) return (NULL) # when display is filtered away
        extract <- paste(if (positions[1] == 1) NULL else "...",
                         extract,
                         if (positions[2] ==
                             stringr::str_length(searchTool$selected()$text))
                           NULL else "...", sep="")
        if (attributes(mainCorpus$query$querystring())$searchterm == "")
          p(extract)
        else
          HTML(annotate_html(extract,  mainCorpus$query$querystring()))
      } else p("select concordance line")
      })
  } else if (identical(config$ContextDisplay$Type, "LineContext")) {
    output$contextview <- renderUI({
      if (! is.null(searchTool$selected())) {
        queryS <- mainCorpus$query$querystring()
        pre <- searchTool$previous()(5)()
        result <- searchTool$selected()
        post <- searchTool$following()(5)()
        if (nrow(pre)>0)
          pre$text <- paste('<span type="context">',
                            htmltools::htmlEscape(pre$text), '<span/>')
        result$text <- annotate_html(result$text, queryS)
        if (nrow(post)>0)
          post$text <- paste('<span type="context">',
                             htmltools::htmlEscape(post$text), '<span/>')
        HTML(knitr::kable(rbind(rbind(pre, result[,colnames(result) %in%
                                                    colnames(pre)]), post)[
                                                      , c("speaker", "text")],
                   format="html", escape=FALSE, row.names=FALSE,
                   table.attr='class="contextTable"'))
      } else p("select concordance line")})
  }



  output$fulltextView <- renderUI(
    if (! is.null(searchTool$selected())) {
      if (attributes(mainCorpus$query$querystring())$searchterm == "")
        p(searchTool$selected()$text)
      else
        HTML(annotate_html(searchTool$selected()$text,
                           mainCorpus$query$querystring()))
    }
    else p("No text selected.")
  )

}
