#' Title
#'
#' @param id
#' @param corpusSelect
#' @param config
#' @param panel
#'
#' @return
#' @export
#'
#' @examples
compareModuleOutput <- function(id, corpusSelect, config, panel=NULL) {

  ns <- NS(id)

  ui <- tagList(bsCollapse(
    bsCollapsePanel(
      "Click to select reference corpus",
      fluidRow(
        corpusUIHorizontalInput(ns("referenceCorpus"), config, corpusSelect,
                                TRUE)),
      value="referencecorpus", style="info"),
    open = NULL, multiple=FALSE),
    fluidRow(
      column(6,
             radioButtons(ns("keynessDir"), label="Keyness",
                          choices = c("Absolute" = "absolute",
                                      "Directed" = "directed"),
                          selected="directed", inline=TRUE))),
    dataTableOutput(ns("compareTable")),
    fluidRow(
      column(4, downloadButton(ns("downloadCompare"), 'Download as .csv'))
    ))

  if (! is.null(panel))
    ui <- tabPanel(panel, ui)

  ui

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param config
#' @param corpusSelect
#' @param restrictions
#' @param mainCorpus
#' @param appControl
#'
#' @return
#' @export
#'
#' @examples
compareModule <- function(input, output, session, config, corpusSelect,
                          restrictions, mainCorpus, appControl) {

  referenceCorpus <- callModule(corpusUIInput, "referenceCorpus", config,
                                corpusSelect, restrictions)

  compareResultsInternal = reactive({

    queryS <- mainCorpus$query$querystring()
    selection <- mainCorpus$selectedCorpus()

    refQueryS <-  referenceCorpus$query$querystring()
    reference <- referenceCorpus$selectedCorpus()

    targetWL <- getWordlist(selection, queryS, mainCorpus$select$controls())
    referenceWL <- getWordlist(reference, refQueryS,
                                  referenceCorpus$select$controls())

    Base.Keywords(targetWL, referenceWL,
                  absolute = input$keynessDir == "absolute")

  })



  compareResults <- reactive(
    if (config$useSubmitButton) {
      mainCorpus$trigger()
      referenceCorpus$trigger()
      isolate(compareResultsInternal())
    } else {
      compareResultsInternal()
    }
  )

  output$compareTable <- DT::renderDataTable({
    if (nrow(filterCorpus(mainCorpus$selectedCorpus(),
                          mainCorpus$query$querystring())$corpus) <1) {
      showNotification("Target corpus empty!", type="warning")
      return(NULL)
    }

    if (nrow(filterCorpus(referenceCorpus$selectedCorpus(),
                          referenceCorpus$query$querystring())$corpus) <1) {
      showNotification("Reference corpus empty!", type="warning")
      return(NULL)
    }

    compareResults()

    }, rownames=FALSE, server = TRUE)

  restrictionSetter <- observeEvent({ input$compareTable_rows_selected }, {

    searchword <- as.character(compareResults()[
      as.numeric(input$compareTable_rows_selected), "word"])

    if (! identical(attributes(mainCorpus$query$querystring())$searchterm, "")) {
      mainCorpus$restrictions$addRestriction(buildRestriction(
        mainCorpus$select$all(),
        mainCorpus$query$querystring())
      )
    }

    appControl$setTab("Search")
    appControl$setSearchMode("KWIC")

    mainCorpus$query$updateFromQuerystring(buildQuerystring(
      searchword, "word",
      !isCaseInsensitive(mainCorpus$query$querystring())))
  })


  output$downloadCompare <- downloadHandler(
    filename = function() "keywords.csv",
    content = function(file) {
      write.csv2(compareResults(), file)
    })

}
