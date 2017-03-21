fluidPage(
  # Application title
  titlePanel("Upload WetQC Data"),
  #selectInput("PLAT", "Platform", c("Xfp"=1,"e24"=2,"e96"=3,"24Legacy"=4), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
  actionButton("goButton","RUN"),
  actionButton("Quit", "Quit"),
  textOutput("MSG"),
  dataTableOutput('DF')
)