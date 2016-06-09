

library(shiny)

shinyUI(fluidPage(
titlePanel("Qualifying Media Run for Instruments"),
mainPanel(
 actionButton('GO','Run',icon=icon("cog",lib='glyphicon')) 
)
)
)
