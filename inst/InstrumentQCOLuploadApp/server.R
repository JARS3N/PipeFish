  require(shiny)
  require(PipeFish)
  server=function(input,output,session) {observeEvent(input$UploadData, {PipeFish::uploadInstQCOL()})})
