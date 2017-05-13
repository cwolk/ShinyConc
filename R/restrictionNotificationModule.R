#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
restrictionNotificationUI <- function(id) {

  ns <- NS(id)

  conditionalPanel(condition = paste0("output['", ns("restrictionactive"),
                                      "']"),
                   wellPanel(fluidRow(
                     column(8, textOutput(ns("restrictionDisplay"))),
                     column(4, actionButton(ns("resetRestrictionButton"),
                                            "Remove restrictions")))))
}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param mainCorpus
#'
#' @return
#' @export
#'
#' @examples
restrictionNotificationModule <- function(input, output, session, mainCorpus) {

  output$restrictionactive <- reactive(
    mainCorpus$restrictions$activeRestrictions$isActive)

  outputOptions(output, 'restrictionactive', suspendWhenHidden=FALSE)

  output$restrictionDisplay <- renderText(
    sprintf("There are additional restrictions active."))

  restrictionRemover <- observeEvent(input$resetRestrictionButton, {
    mainCorpus$restrictions$clearRestrictions()
  })

}
