library(shiny)
library(PipeFish)
library(dplyr)
library(rio)
library(rmarkdown)
shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  
  observe({
    if(input$BB > 0 ){
      DIR<-choose.dir()
      if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
      PipeFish::pKa(input$pHFluor,input$MFBatch,input$Platform,DIR)
    }
  })
})


