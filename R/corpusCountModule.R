#' Title
#'
#' @param id
#' @param panel
#'
#' @return
#' @export
#'
#' @examples
countModuleOutput <- function(id, panel=NULL) {

  ns <- NS(id)

  ui <- tagList(radioButtons(ns("countType"), label="Display",
                             choices=c("Frequency table" = "Table",
                                       "Wordlist" = "Wordlist"),
                             selected = "Wordlist", inline=TRUE),
                dataTableOutput(ns("countTable")),
                fluidRow(
                  column(4, downloadButton(ns('downloadCount'),
                                           'Download as .csv'))
                )
  )

  if (! is.null(panel)) {
    ui <- tabPanel(panel, ui)
  }

  ui

}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param mainCorpus
#' @param config
#' @param appControl
#'
#' @return
#' @export
#'
#' @examples
countModule <- function(input, output, session, config, mainCorpus,
                        appControl) {

  countResultsInternal <- reactive({

    queryS <- mainCorpus$query$querystring()
    selection <- mainCorpus$selectedCorpus()

    result <- runWithRegExCatch(switch(input$countType,
                                       "Table" = getCounts,
                                       "Wordlist" = getWordlist),
                                selection, queryS, mainCorpus$select$controls())


    return(result)

  })

  countResults <- reactive(
    if (config$useSubmitButton) {
      mainCorpus$trigger()
      input$countType
      isolate(countResultsInternal())
    } else {
      countResultsInternal()
    }
  )

  output$countTable <- DT::renderDataTable( {

    if (nrow(mainCorpus$selectedCorpus()$corpus) < 1) {
      showNotification("Corpus empty!", type="warning")
      return(NULL)
    }

    data <- countResults()

    if (is.null(data)){
      showNotification("Please enter search terms", type="warning")
      return(NULL)
    }

    dt <- datatable(data, selection = 'single', rownames=FALSE)
    if ("Frequency" %in% colnames(data))
      dt <- dt %>% formatStyle(
        "Frequency", background = styleColorBar(c(0, sum(data$Frequency)),
                                                "lightblue"))
    if ("Pairs" %in% colnames(data))
      dt <- dt %>% formatStyle(
        "Pairs", background = styleColorBar(c(0, nrow(mainCorpus$selectedCorpus())),
                                            "lightblue"))
    if ("Texts" %in% colnames(data))
      dt <- dt %>% formatStyle(
        "Texts", background = styleColorBar(c(0, nrow(mainCorpus$selectedCorpus())),
                                            "lightblue"))
    if ("per hundred words" %in% colnames(data))
      dt <- dt %>% formatStyle(
        "per hundred words", background = styleColorBar(data$`per hundred words`,
                                                        "lightblue"))
    dt

  }, server = TRUE)

  output$downloadCount <- downloadHandler(
    filename = function() switch(input$countType,
                                 "Wordlist" = "wordlist.csv",
                                 "Table" = "frequency.csv"),
    content = function(file) {
      write.csv2(countResults(), file)
    })

  restrictionSetter <- observeEvent({ input$countTable_rows_selected }, {

    searchword <- as.character(countResults()[
      as.numeric(input$countTable_rows_selected), "Token"])

    mainCorpus$restrictions$addRestriction(buildRestriction(
      mainCorpus$select$all(),
      mainCorpus$query$querystring())
    )

    appControl$setTab("Search")
    appControl$setSearchMode("KWIC")

    mainCorpus$query$updateFromQuerystring(buildQuerystring(
      searchword, "word",
      !isCaseInsensitive(mainCorpus$query$querystring())))
  })

}
