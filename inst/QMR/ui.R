

library(shiny)

shinyUI(fluidPage(
titlePanel("Qualifying Media Run for Instruments"),
mainPanel(
 actionButton('GO','Run',icon=icon("cog",lib='glyphicon')) ,
 br(),br(),br(),
 actionButton('Quit','Quit',icon=icon('remove-sign',lib='glyphicon'))
)
)
)
