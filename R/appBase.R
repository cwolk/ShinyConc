#' Observer to update a CheckboxGroup from a reactiveValue
#'
#' @param id ID
#' @param input input
#' @param session session
#' @param updates updates reactiveValues
#'
#' @return observeEvent that performs the updates and sets ID in the updates reactiveValues to NULL
#' @export
#'
#' @examples
checkboxGroupUpdater <- function(id, input, session, updates) {
  observeEvent(updates[[id]],
               if (!is.null(updates[[id]])) {
                 updateCheckboxGroupInput(session, id, selected=updates[[id]])
                 updates[[id]] <- NULL
               })
}

#' Observer to update a Checkbox from a reactiveValue
#'
#' @param id ID
#' @param input input
#' @param session session
#' @param updates updates reactiveValues
#'
#' @return observeEvent that performs the updates and sets ID in the updates reactiveValues to NULL
#' @export
#'
#' @examples

checkboxUpdater <- function(id, input, session, updates) {
  observeEvent(updates[[id]],
               if (!is.null(updates[[id]])) {
                 updateCheckboxInput(session, id, value=updates[[id]])
                 updates[[id]] <- NULL
               })
}

#' Observer to update a TextInput from a reactiveValue
#'
#' @param id ID
#' @param input input
#' @param session session
#' @param updates updates reactiveValues
#'
#' @return observeEvent that performs the updates and sets ID in the updates reactiveValues to NULL
#' @export
#'
#' @examples

textUpdater <- function(id, input, session, updates) {
  observeEvent(updates[[id]],
               if (!is.null(updates[[id]])) {
                 updateTextInput(session, id, value=updates[[id]])
                 updates[[id]] <- NULL
               })
}

#' Observer to update a radioButton from a reactiveValue
#'
#' @param id ID
#' @param input input
#' @param session session
#' @param updates updates reactiveValues
#'
#' @return observeEvent that performs the updates and sets ID in the updates reactiveValues to NULL
#' @export
#'
#' @examples
radioButtonUpdater <- function(id, input, session, updates) {
  observeEvent(updates[[id]],
               if (!is.null(updates[[id]])) {
                 updateRadioButtons(session, id, selected=updates[[id]])
                 updates[[id]] <- NULL
               })
}



#' Create an Updating observer,
#'
#' @param type type of the widget (character)
#' @param id id of widget
#' @param input input
#' @param session session
#' @param updates updates
#'
#' @return NULL
#' @export
#'
#' @examples
selectorUpdater <- function(type, id, input, session, updates) {
  switch(type,
         "radioButton"=radioButtonUpdater(id, input,session, updates),
         "text"=textUpdater(id, input,session, updates),
         "checkbox"=checkboxUpdater(id, input,session, updates),
         "checkboxGroup"=checkboxGroupUpdater(id, input,session, updates))
}
