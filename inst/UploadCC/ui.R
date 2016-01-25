library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Upload XFp Data"),
  actionButton("goButton","RUN"),
  textOutput("MSG")
))

