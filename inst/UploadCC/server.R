library(shiny)
library(RMySQL)
library(PipeFish)
library(dplyr)
library(XML)

shinyServer(function(input, output) {
    observe({
        output$MSG <- renderText("Ready")
        if(input$goButton > 0){
            output$MSG <- renderText("Select Directory")
            DIR<-choose.dir();
            output$MSG <- renderText("Munging Data...")
           DF<- DIR %>%
                list.files(path=.,pattern='asyr',full.names=TRUE) %>%
                lapply(.,XML::xmlTreeParse) %>%
                lapply(.,PipeFish::Collect) %>%
                lapply(.,PipeFish::ComboAssay) %>%
                rbind_all(.) %>%
               as.data.frame(.)
               output$DF<-shiny::renderDataTable(DF)
           output$MSG <- renderText("Communicating with Database")
           ConnectInfo<-DataStash::Triton()
           my_db <- dbConnect(RMySQL::MySQL(),
                              dbname=ConnectInfo[1],
                              user=ConnectInfo[2],
                              password=ConnectInfo[3],
                              host=ConnectInfo[4],
                              port=as.numeric(ConnectInfo[5]))
           output$MSG <- renderText("Writing to Database")
            dbWriteTable(my_db, name="xfpwetqc", value=DF,append=TRUE,overwrite = FALSE,row.names=FALSE)
            dbDisconnect(my_db)
            output$MSG <- renderText("Complete")
            }})
})
