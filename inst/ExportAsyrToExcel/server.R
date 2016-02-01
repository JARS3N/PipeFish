
library(shiny)
library(PipeFish)
shinyServer(function(input,output,session) {

  stuffHappens<-  observeEvent(input$EXP,
    {fldr<-choose.dir()
    PipeFish::Outandsave(fldr)
    output$session <- renderText("Script Complete")
    })
})
