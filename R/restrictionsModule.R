#' build a restriction
#'
#' @param restriction
#' @param query
#'
#' @return
#' @export
#'
#' @examples
buildRestriction <- function(restriction, querystring) {
  list(
    restriction = restriction,
    querystring = querystring
  )
}

valueOrNone <- function(value) {
  ifelse(is.null(value) | value == "", "<None>", value)
}

restrictionsToDF <- function(restrictions) {
  if (length(restrictions)<1)
    return(NULL)
  result <- data.frame(ID=1:length(restrictions))
  result$description <- sapply(restrictions, function(x) {
    paste0(paste0(names(x$restriction), ": ",
                  as.character(x$restriction)), collapse = ", ")})
  result$query <- sapply(restrictions, function(x) {
    paste0("Query: ", valueOrNone(attributes(x$querystring)$searchterm))
  })
  result
}

#' Restrictions module UI
#'
#' @param id module ID
#' @param selectionUI
#' @param ... other inputs passed to selectionUI
#'
#' @return
#' @export
#'
#' @examples
restrictionsFieldInput <- function(id, selectionUI, ...) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("restrictionsTable")),
    fluidRow(column(7, selectionUI(ns("selectRestriction"), ...)),
             column(4, queryFieldInput(ns("queryRestriction"), "Filter",
                                       FALSE, useSubmitButton=FALSE),
                    column(5,actionButton(ns("restrictionConfirm"),
                                          label = "Add Restriction")),
                    column(5,actionButton(ns("restrictionDelete"),
                                          label = "Delete Restriction"))))
  )
}

#' Restrictions Module server function
#'
#' @param input Input
#' @param output Output
#' @param session Session
#' @param selectionModule selection Module function
#' @param ... other paramters passed to selectionModule
#'
#' @return
#' @export
#'
#' @examples
restrictionsField <- function(input, output, session, selectionModule, ...) {

  active <- reactiveValues(restrictions=list(), isActive=FALSE)

  restrictionQuery <- callModule(queryField, "queryRestriction")

  restrictionSelect <- callModule(selectionModule, "selectRestriction", ...)

  restrictionsTable <- reactive(
    restrictionsToDF(active$restrictions))

  output$restrictionsTable <- DT::renderDataTable({
    datatable(restrictionsTable(),selection = 'single', rownames=FALSE,
              options = list(dom = 't'))
  })

  restrictionSelected <- reactive({
    length(input$restrictionsTable_rows_selected) > 0
  })

  setRestriction <- function(restriction, which = NULL) {
    if (is.null(which))
      which <- length(active$restrictions) + 1
    active$restrictions[[which]] <- restriction
    active$isActive <- TRUE
  }

  deleteRestriction <- function(which) {
    active$isActive <- active$isActive & (length(active$restrictions) > 1) &
      which %in% 1:length(active$restrictions)
    active$restrictions[[which]] <- NULL
    #browser()
  }

  clearRestrictions <- function() {
    active$restrictions <- list()
    active$isActive <- FALSE
  }

  processRestrictions <- function(corpus) {
    if (active$isActive) {
      #browser()
      for (i in 1:length(active$restrictions)) {
        #browser()
        corpus <- applyRestriction(
          corpus,
          active$restrictions[[i]]$restriction,
          active$restrictions[[i]]$querystring)
      }
    }
    corpus
  }

  observeEvent(input$restrictionConfirm, {
    target <- if (restrictionSelected())
      input$restrictionsTable_rows_selected else NULL
    #browser()
    setRestriction(buildRestriction(restrictionSelect$all(),
                                    restrictionQuery$querystring()),
                   target)

  })

  observeEvent(input$restrictionDelete, {
    if (restrictionSelected())
      deleteRestriction(input$restrictionsTable_rows_selected)
  })


  observeEvent(input$restrictionsTable_rows_selected, {
    if (length(input$restrictionsTable_rows_selected) > 0) {
      #browser()
      selected_restr <- active$restrictions[[
        input$restrictionsTable_rows_selected]]$restriction
      selected_query <- active$restrictions[[
        input$restrictionsTable_rows_selected]]$querystring
      for (restr in names(selected_restr)) {
        restrictionSelect$updates[[restr]] <- selected_restr[[restr]]
      }
      restrictionQuery$updates$searchterm <- attributes(selected_query)$searchterm
      restrictionQuery$updates$mode <- attributes(selected_query)$mode
      restrictionQuery$updates$casesensitive <- attributes(selected_query)$casesensitive

    }
  })

  return (list(activeRestrictions=active,
               addRestriction = function(x) setRestriction(x, NULL),
               clearRestrictions = clearRestrictions,
               processRestrictions = processRestrictions))
}
