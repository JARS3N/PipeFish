
library(shiny)
library(PipeFish)
library(dplyr)
library(rio)
library(rmarkdown)
shinyServer(function(input, output, session) {
  
  observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  
  observe({
    if(input$BB > 0 ){
      DIR<-choose.dir()
      PipeFish::pKa2(input$pHFluor,input$MFBatch,DIR)
  })
})
