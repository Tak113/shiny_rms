library(shiny)
library(shinydashboard)

#import files3.5
source('ui.R', local = TRUE)
source('server.R', local = TRUE)
source('global.R', local = TRUE)

shinyApp(
  ui = ui,
  server = server,
  onStart = global # function that will be called before the app is actually run
)