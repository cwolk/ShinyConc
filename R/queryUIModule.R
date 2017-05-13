#' Returns empty updates reactiveValues for use with Query module
#'
#' @return reactiveValues with fields searchterm, mode, and casesensitive, all
#' set to NULL
#' @export
#'
#' @examples
queryUpdaterValues <- function() {
  do.call(reactiveValues, list(searchterm=NULL, mode=NULL, casesensitive=NULL))
}

#' Query field module UI function
#'
#' @param id Shiny Module ID
#' @param label Query label
#' @param isMain Indicate whether this is the main query or an auxilliary one (e.g. for reference corpus selection in keyword view)
#' @param useSubmitButton Should the search be interactive or wait for the submit button to be clicked?
#'
#' @return query field module UI
#' @export
queryFieldInput <- function(id, label="Search/Filter", isMain=TRUE, useSubmitButton=TRUE) {
  ns <- NS(id)
  tagList(
    h3(label),
    fluidRow(column((if (useSubmitButton & isMain) 10 else 12),
                    textInput(ns("searchterm"), label=NULL, value = "")),
             if (useSubmitButton & isMain)
               column(2, actionButton(ns("submitButton"), icon("search")))),
    radioButtons(ns("mode"), label=h3("Search mode"),
                 choices= list("string" = "string",
                               "word" = "word",
                               "regular expression" = "regex"),
                 selected="word"),
    checkboxInput(ns("casesensitive"), label = "case-sensitive", value = FALSE)
  )
}

#' Query field module server function
#'
#' @param input input
#' @param output output
#' @param session session
#' @param updates A reactive value that can be used to modify the module state
#'
#' @return List containing reactives: searchterm, mode, casesensitive, submitButton, querystring
#' @export
queryField <- function(input, output, session) {

  updates <- reactiveValues (searchterm=NULL, mode=NULL, casesensitive=NULL)

  textUpdater("searchterm", input, session, updates)

  radioButtonUpdater("mode", input, session, updates)

  checkboxUpdater("casesensitive", input, session, updates)

  updateFromQuerystring <- function(qs) {
    updates$searchterm <- attributes(qs)$searchterm
    updates$mode <- attributes(qs)$mode
    updates$casesensitive <- attributes(qs)$casesensitive
  }

  return(list(
    searchterm=reactive(input$searchterm),
    mode=reactive(input$mode),
    casesensitive=reactive(input$casesensitive),
    submitButton=reactive(input$submitButton),
    querystring = reactive(buildQuerystring(input$searchterm, input$mode,
                                            input$casesensitive)),
    updates=updates,
    updateFromQuerystring=updateFromQuerystring
  ))
}
