

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("PipeFish::Upload Instrument QC Light Leak Data"),
    mainPanel(
      p("Select directory containing xlsx files from light leak utility test"),
      actionButton('send', "Upload Data", icon = icon("cog", lib = "glyphicon"), width = NULL)
    )
  )
)
