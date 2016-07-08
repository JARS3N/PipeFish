
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
       if(input$CB==TRUE){PipeFish::torquemada(DIR)}
     PipeFish::asyr_pKa(input$pHFluor,input$MFBatch,input$Platform,DIR)
    }
  })
})
