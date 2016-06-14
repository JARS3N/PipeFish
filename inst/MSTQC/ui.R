
library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Upload MST QC DATA"),
  checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
  checkboxInput("CB2", label = "Write data to csv", value = FALSE),
  checkboxInput("CB3", label = "Upload data to database", value = FALSE),
  actionButton("goButton","RUN"),
  actionButton('Quit','Quit'),
  br(),br(),br(),
  textOutput("MSG"),
  dataTableOutput('DF')
))
