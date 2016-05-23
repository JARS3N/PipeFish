library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Upload XFp Data"),
  selectInput("PLAT", "Platform", c("Xfp"=1,"e24"=2,"e96"=3), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
  actionButton("goButton","RUN"),
  textOutput("MSG"),
  dataTableOutput('DF')
))

