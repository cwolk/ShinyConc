#' Vertical organization for corpus selection and query UI
#'
#' @param id id
#' @param config ShinyConc config
#' @param corpusSelect corpus
#' @param editRestrictions whether the restriction handling interface be
#' displayed
#'
#' @return
#' @export
#'
#' @examples
corpusUIVerticalInput <- function(id, config, corpusSelect, editRestrictions=FALSE) {

  ns <- NS(id)

  tagList(selectionFieldInputFromConfig(ns("select"), config, corpusSelect),
          (if (editRestrictions)
             tagList(
               actionButton(inputId = ns("editRestrictionsButton"),
                          "Edit Restrictions"),
               bsModal(id = "restrictonsModal",
                       title = "Edit Restrictions",
                       trigger = ns("editRestrictionsButton"),
                       size="large", {
                         restrictionsFieldInput(ns("restrictionsList"),
                                                selectionFieldInputFromConfig,
                                                config=config,
                                                corpusSelect= corpusSelect)
                       }))
           else NULL),
          queryFieldInput(ns("query"), "Search / Filter", TRUE,
                          useSubmitButton=config$useSubmitButton))
}

#' Vertical organization for corpus selection and query UI
#'
#' @param id id
#' @param config ShinyConc config
#' @param corpusSelect corpus
#' @param extraButton whether an extra display button should be displayed
#'
#' @return
#' @export
#'
#' @examples
corpusUIHorizontalInput <- function(id, config, corpusSelect, extraButton = NULL) {

  ns <- NS(id)

  fluidRow(
    column(7, selectionFieldInputFromConfig(
      ns("select"), config = config, corpusSelect = corpusSelect)),
    column(4, queryFieldInput(ns("query"), "Filter",
                              FALSE, useSubmitButton=config$useSubmitButton),
           (if (!is.null(extraButton))
             actionButton(ns("runButton"), "Compare") else NULL)))
}

#' CorpusUI Module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param config ShinyConc config
#' @param corpusSelect corpus
#' @param restriction restriction module (result)
#'
#' @return CorpusUI Module
#' @export
#'
#' @examples
corpusUIInput <- function(input, output, session, config,
                          corpusSelect, restriction = NULL) {

  query <- callModule(queryField, "query")

  select <- callModule(selectionField, "select", config=config,
                           corpusSelect=corpusSelect)

  trigger <- reactive({
    ## buttons
    if (config$useSubmitButton)
      query$submitButton()
    input$runButton
    ## selections and restrictions
    select$controls()
    select$selection()
    restrictions$activeRestrictions$restrictions
    restrictions$activeRestrictions$isActive
    TRUE
  })

  if (!is.null(restriction)) {
    restrictions <- if (identical (restriction, TRUE))  {
       callModule(restrictionsField, "restrictionsList", selectionField,
                  config=config, corpusSelect= corpusSelect)
      } else restriction
  }

  corpus <- reactive({
    corpusSelect(select$ShinyConc.Corpus())
  })

  selectedCorpus <- reactive({
    selection <-  corpus()
    selection <- restrictions$processRestrictions(selection)
    applyRestriction(selection, select$selection(), NULL)
  })

  return(list(query = query,
              select = select,
              restrictions = restrictions,
              fullcorpus = corpus,
              selectedCorpus = selectedCorpus,
              trigger = trigger))

}
