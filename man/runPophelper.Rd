% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script.R
\name{runPophelper}
\alias{runPophelper}
\title{Launch interactive pophelper shiny application}
\usage{
runPophelper(display.mode = "normal", launch.browser = TRUE, ...)
}
\arguments{
\item{display.mode}{Display mode. See \code{\link{runApp}}.}

\item{launch.browser}{Logical indicating if the app is to be launched in the system browser.}

\item{...}{Further arguments are passed to \code{\link{runApp}}.}
}
\value{
This function does not return anything
}
\description{
Launches interactive shiny session in the browser.
}
\examples{
\dontrun{
library(pophelperShiny)
runPophelper()
}
}
\seealso{
\code{\link{runApp}}
}
