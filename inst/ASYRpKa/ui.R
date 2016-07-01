library(shiny)

shinyUI(fluidPage(
  titlePanel("PipeFish::pKa for 96 asyr only"),
  br(),
  mainPanel(
    textInput('pHFluor',"pH Fluorophore", value = "Fluorophore_Validation_pKa", width = NULL),
    textInput('MFBatch',"Multi Fluor Batch", value = "NA", width = NULL),
    br(),
    actionButton("BB","Run Analysis"),
    br(),
    actionButton("Quit", "Quit"),
    br(),
    textOutput("session"),
    tableOutput("test1")
  )
  
)
)
