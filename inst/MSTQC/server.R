
library(shiny)
library(RMySQL)
library(PipeFish)
library(dplyr)
ConnectInfo<-DataStash::Triton()
shinyServer(function(input, output,session) {
 session$onSessionEnded(function() {
    stopApp()
  })
 
 observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  observe({
    output$MSG <- renderText("Ready")
    if(input$goButton > 0){
      output$MSG <- renderText("Select Directory")
      DIR<-choose.dir();
      if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
      output$MSG <- renderText("Munging Data...")
      DF<- DIR %>%
        list.files(path=.,pattern='.xlsx',full.names=TRUE) %>%
        lapply(.,PipeFish::CleanMST) %>% 
        lapply(.,PipeFish::MST) %>%
        rbind_all(.) %>%
        as.data.frame(.)
        output$DF<-shiny::renderDataTable(DF)
        output$MSG <- renderText("Complete")
        if (input$CB2==TRUE){write.csv(DF,file.path(DIR,'MSToutput.csv'),row.names=FALSE)}
        if (input$CB3==TRUE){
            ConnectInfo<-DataStash::Triton()
            my_db <- rmysqlCon()
            dbWriteTable(my_db, name='mstqc', value=DF,append=TRUE,overwrite = FALSE,row.names=FALSE)
            dbDisconnect(my_db)
        }
    }})
})
