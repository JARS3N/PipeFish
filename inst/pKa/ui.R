library(shiny)

shinyUI(fluidPage(
  titlePanel("PipeFish::pKa"),
  br(),
  mainPanel(
    textInput('pHFluor',"pH Fluorophore", value = "Fluorophore_Validation_pKa", width = NULL),
    textInput('MFBatch',"Multi Fluor Batch", value = "NA", width = NULL),
    selectInput("Platform", label = "Platform", 
                choices = list("XFe24" = 1, "XFe96" = 2, "XF24" = 3,"XFp"=4), 
                selected = 1),
    checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
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
