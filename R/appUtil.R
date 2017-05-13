#' Load packages and install them if needed
#'
#' @param packages vector of package names
#'
#' @return Nothing
#' @export
#'
#' @examples loadOrInstall(c("shiny", "shinyBS"))
loadOrInstall <- function(packages) {
  for (package in packages) {
    if (! (require(package, character.only = TRUE))) {
      install.packages(package)
      library(package, character.only=TRUE)
    }
  }
}


#' Run a function and catch any regex errors, returning an error messge data frame instead
#'
#' @param fun function executing the regex search
#' @param ... other arguments passed to fun
#'
#' @return the result of fun, or a data frame containing an error message if the execution of fun causes an regex error
#' @export
#'
#' @examples runWithRegExCatch(grep, ".*(", "should lead to an error")
runWithRegExCatch <- function(fun, ...) {
  tryCatch(fun(...), error = function(e) {
    #if (stringr::str_detect(e, "U_REGEX"))
      return(data.frame(Message=c(
        "Search error. Please check regular expression",
        "Details:", toString(e))))
    #else stop(e)
  })
}
