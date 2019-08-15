# POPHELPER SHINY

#' @title Launch interactive pophelper shiny application
#' @description Launches interactive shiny session in the browser.
#' @param display.mode Display mode. See \code{\link{runApp}}.
#' @param launch.browser Logical indicating if the app is to be launched in the system browser.
#' @param ... Further arguments are passed to \code{\link{runApp}}.
#' @return This function does not return anything
#' @seealso \code{\link{runApp}}
#' @examples
#' \dontrun{
#' library(pophelperShiny)
#' runPophelper()
#' }
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom gridExtra grid.arrange
#' @importFrom htmlwidgets JS
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shinyAce aceEditor
#' @importFrom shinyBS bsTooltip
#' @importFrom shinythemes shinytheme
#' @importFrom shinyWidgets pickerInput
#' @importFrom tidyr gather spread
#' @importFrom viridisLite viridis inferno magma plasma
#' @importFrom writexl write_xlsx
#' @export
#' 
runPophelper <- function(display.mode="normal",launch.browser=TRUE,...) {
  appDir <- system.file("app", package="pophelperShiny")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `pophelperShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir,display.mode=display.mode,launch.browser=launch.browser,...)
}
