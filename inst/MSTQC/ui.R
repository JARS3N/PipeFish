library(shiny)

shinyUI(fluidPage(
        titlePanel("PipeFish::MSTQC"),
        br(),
        mainPanel(
            textInput('expnm',"Name of Experiment", value = "MST QC Analysis", width = NULL),
            checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
            br(),
            actionButton("BB","Run Analysis"),
            br(),
            textOutput("session"),
            tableOutput("test1")
        )

)
 )
