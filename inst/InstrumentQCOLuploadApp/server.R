  require(shiny)
  require(PipeFish)
  fluidPage(p(msg),mainPanel(actionButton("UploadData", "UploadData"),textOutput("session")))
