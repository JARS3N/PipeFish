  msg<-"select Directory of xlsx files to upload"
  require(shiny)
  require(PipeFish)
  fluidPage(p(msg),mainPanel(actionButton("UploadData", "UploadData"),textOutput("session")))
