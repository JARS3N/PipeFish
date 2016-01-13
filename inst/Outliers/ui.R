library(shiny)

shinyUI(fluidPage(
        titlePanel("PipeFish::Outliers"),
        br(),
        mainPanel(
            textInput('expnm',"Name of Experiment", value = "OutlierAnalysis", width = NULL),
            br(),
            actionButton("BB","Run Analysis"),
            br(),
            textOutput("session"),
            tableOutput("test1")
        )

)
 )
