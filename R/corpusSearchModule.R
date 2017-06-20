#' Title
#'
#' @param id
#' @param panel
#'
#' @return
#' @export
#'
#' @examples
searchModuleOutput <- function(id, panel=NULL) {

  ns <- NS(id)

  ui <- tagList(
    fluidRow(
      column(6, radioButtons(ns("searchType"), label="Display", inline=TRUE,
        choices=list("KWIC" = "KWIC", "Data" = "Data"),
        selected="KWIC")),
      column(6, textOutput(ns("Summary")))),
    dataTableOutput(ns("KWIC")),
    fluidRow(
      column(4, downloadButton(ns('downloadSearch'), 'Download as .csv'))
    ))

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
#' @param config
#' @param mainCorpus
#' @param appControl
#'
#' @return
#' @export
#'
#' @examples
searchModule <- function(input, output, session, config, mainCorpus,
                        appControl) {

  resultInternal <- reactive({
    queryS <- mainCorpus$query$querystring()
    selection <- mainCorpus$selectedCorpus()

    result <- runWithRegExCatch(switch(input$searchType,
                                       "Data" = filterCorpus,
                                       "KWIC" = getKWIC), selection, queryS,
                                mainCorpus$select$controls())

    if (input$searchType == "Data") {
      result <- result$corpus
    }

    return (result)
  })

  subcorpusSize <- reactive({
    getWordcount(mainCorpus$selectedCorpus(),
                                              mainCorpus$select$controls())
    })

  hits <- reactive({
    if (identical(colnames(result()), "Results"))
      0 else nrow(result())
  })

  result <- reactive(
    if (config$useSubmitButton) {
      mainCorpus$trigger()
      input$searchType
      isolate(resultInternal())
    } else {
      resultInternal()
    }
  )

  output$KWIC <- DT::renderDataTable({
    data <- result()
    if (is.null(data)){
      showNotification("Please enter search terms", type="warning")
      return(NULL)
    }

    #browser()

    if (input$searchType == "Data" & all(config$SearchTool$Data$DisplayColumns
                                         %in% colnames(data))) {
      collength <- 30
      # TODO: make configurable
      for (col in config$SearchTool$Data$DisplayColumns)
        data[,col] <- str_c(str_sub(data[,col], 1, collength),
                        ifelse(str_length(data[,col]) > collength, "...", ""))
    }

    if (input$searchType == "Data")
      data <- data[,config$SearchTool$Data$DisplayColumns,drop=FALSE]
    if (input$searchType == "KWIC")
      data <- data[,c(config$SearchTool$KWIC$DisplayExtraColumns, "left",
                      "center", "right"),drop=FALSE]
    dt <- datatable(data, selection = 'single', rownames=FALSE,
                    options = list(autoWidth = FALSE,
                                   columnDefs = createClassesMap(data, list(
                                     c("left-KWIC", "left"),
                                     c("center-KWIC", "center"),
                                     c("right-KWIC", "right")
                                   ))))
    return(dt)
  }, server=TRUE)

  output$Summary <- renderText({
    switch(
    input$searchType,
    "KWIC" = sprintf("%d tokens found (%d words in selection)",
                     hits(), subcorpusSize()),
    "Data" = sprintf("%d entries found (%d words in selection)",
                     hits(), subcorpusSize())
  )})

  output$downloadSearch <- downloadHandler(
    filename = function() switch(input$searchType,  "KWIC" = "KWIC.csv",
                                 "Data"= "data.csv"),
    content = function(file) {
      write.csv2(result(), file)
    })

  return(list(
    selected = reactive ({
      #browser()
      if ((! is.null(result())) && (nrow(result()) > 0) &&
          (! is.null(input$KWIC_rows_selected))) {
        if (input$searchType == "Data") {
          result()[as.numeric(input$KWIC_rows_selected),]
        } else {
          result()[as.numeric(input$KWIC_rows_selected),]
#          mainCorpus$fullcorpus()$corpus[mainCorpus$fullcorpus()$corpus == result()[as.numeric(input$KWIC_rows_selected), "ShinyConc.ID"],][1,]
        }
      } else NULL
    }),
    mode=reactive(input$searchType)
  ))


}
