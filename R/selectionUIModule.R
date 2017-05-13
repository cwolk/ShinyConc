uiElement <- c("radioButtons" = radioButtons,
               "checkboxGroup" = checkboxGroupInput,
               "select" = function(...) selectInput(..., selectize=FALSE,
                                                    multiple=TRUE))

updateElement <- c("radioButtons" = updateRadioButtons,
                   "checkboxGroup" = updateCheckboxGroupInput,
                   "select" = updateSelectInput)


SelectorToUI <- function(id, selector, corpus, ns) {

  outvals <- if (identical (selector$values, "verbatim"))
    getValues(corpus, id) else unlist(selector$values)

  default <- if(is.numeric(selector$default))
    outvals[selector$default] else
      if (is.character(selector$default))
        selector$default else
          if (identical(selector$default, TRUE))
            outvals else NULL

  tagList(uiElement[[selector$type]](
    ns(id), label = h3(selector$label), choices = outvals, selected=default))
}

SelectorOrderToUI <- function(selectorOrder, selectors, corpus, ns) {
  tagList(
    lapply(selectorOrder, function(selectorList) {
      fluidRow(lapply(selectorList, function(selectr) {
        column(selectr$size, SelectorToUI(selectr$Selector,
                                          selectors[[selectr$Selector]],
                                          corpus, ns))
      }))
    })
  )
}

getValues <- function(corpus, column) {
  return(sort(unique(corpus$corpus[,column])))
}


#' create selection module ui
#'
#' @param id id
#' @param config ShinyConc config
#' @param corpusSelect corpus
#'
#' @return selection module ui, as specified in the ShinyConc config
#' @export
#'
#' @examples
selectionFieldInputFromConfig <- function(id, config, corpusSelect) {
  ns <- NS(id)
  corpus <- corpusSelect(config$Selectors$ShinyConc.Corpus$default)
  SelectorOrderToUI(config$SelectorOrder, config$Selectors, corpus, ns)
}


#' selection module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param config ShinyConc config file
#' @param corpus corpus
#'
#' @return
#' @export
#'
#' @examples
selectionField <- function(input, output, session, config, corpusSelect) {

  updates <- reactiveValues()

  flatSelectorOrder <- unlist(sapply(config$SelectorOrder, sapply,
                                     function(x) x$Selector))

  # browser()
  restrictors <- flatSelectorOrder[
    flatSelectorOrder %in%
      colnames(corpusSelect(config$Selectors$ShinyConc.Corpus$default)$corpus)]

  extras <- flatSelectorOrder[
    ! flatSelectorOrder %in%
      colnames(corpusSelect(config$Selectors$ShinyConc.Corpus$default)$corpus)]

  for (selectr in flatSelectorOrder) {
    updates[[selectr]] <- NULL
    selectorUpdater(config$Selectors[[selectr]]$type, selectr, input, session,
                    updates)
  }

  lapply(1:length(flatSelectorOrder), function(selectNum) {
    sel <- flatSelectorOrder[selectNum]
    if (config$Selectors[[sel]]$cascade) {
      observeEvent({sapply(1:selectNum, function(x)
        input[[flatSelectorOrder[x]]]) }, {

          selectedvalues <- input[[sel]]
          ## TODO: Implement cascade for fixed
          #browser()
          pcorp <- applyRestriction(corpusSelect(input$ShinyConc.Corpus),
                                    selection()[
                                      flatSelectorOrder[1:(selectNum - 1)]],
                                    NULL)
          newvals <- getValues(pcorp, sel)
          newselvals <- selectedvalues[selectedvalues %in% newvals]
          if (length(newselvals) < 1)
            newselvals <- newvals
          ## TODO: I'm a bit uncertain about the best behavior here

#          browser()
          updateElement[[config$Selectors[[sel]]$type]](session, sel,
                                                        choices=newvals,
                                                        selected=newselvals)
        })

    }
  })


  selectors <- lapply(flatSelectorOrder, function(x) reactive(input[[x]]))
  names(selectors) <- flatSelectorOrder

  selection <- reactive({
    result <- lapply(restrictors, function(x) input[[x]])
    names(result) <- restrictors
    result
  })

  controls <- reactive({
      result <- lapply(extras, function(x) input[[x]])
      names(result) <- extras
      result
  })

  all <- reactive({
    result <- lapply(flatSelectorOrder, function(x) input[[x]])
    names(result) <- flatSelectorOrder
    result

  })

  return(
    c(list(updates=updates),
      selectors,
      list(selection=selection,
      controls=controls,
      all=all)
    ))
}
