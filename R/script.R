# POPHELPER SHINY

#' @title runPophelper
#' @description Launches interactive shiny session in the browser.
#' @export
#' 
runPophelper <- function() {
  appDir <- system.file("app", package="pophelperShiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir,display.mode="normal")
}