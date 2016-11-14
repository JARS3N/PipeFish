library(shiny)
library(RMySQL)
library(PipeFish)
library(dplyr)
library(XML)


shinyServer(function(input, output) {
  observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  observe({
    output$MSG <- renderText("Ready")
    if(input$goButton > 0){
      output$MSG <- renderText("Select Directory")
      DIR<-choose.dir();
      output$MSG <- renderText("Munging Data...")

      DF<-DIR %>% 
        list.files(path=.,pattern='asyr',full.names=TRUE) %>%
        lapply(.,XML::xmlTreeParse) %>% 
        lapply(.,PipeFish::Collect) %>% 
        lapply(.,PipeFish::whichAssay) %>% 
        dplyr::bind_rows(.) %>%
        select(.,-O2.IntialReferenceDelta,-pH.IntialReferenceDelta) %>%
        as.data.frame(.)
        
      
      output$DF<-shiny::renderDataTable(DF)
      output$MSG <- renderText("Communicating with Database")
      output$MSG <- renderText("Writing to Database")
      PipeFish::UploadsCC(DF)
      output$MSG <- renderText("Complete")
    }})
})
