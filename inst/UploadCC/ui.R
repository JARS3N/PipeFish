library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  actionButton("goButton","RUN"),
  renderPrint(output$TXT)
))
