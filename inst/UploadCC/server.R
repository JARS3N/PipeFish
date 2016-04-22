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
           ConnectInfo<-readRDS(file.path((.libPaths()[1]),"DataStash","DBinfo.RDS"))
           my_db <- dbConnect(RMySQL::MySQL(),
                              dbname=ConnectInfo["dbname"],
                              user=ConnectInfo["user"],
                              password=ConnectInfo["password"],
                              host=ConnectInfo["host"],
                              port=as.numeric(ConnectInfo["port"]))
           output$MSG <- renderText("Writing to Database")
            dbWriteTable(my_db, name="xfpwetqc", value=DF,append=TRUE,overwrite = FALSE,row.names=FALSE)
            dbDisconnect(my_db)
            output$MSG <- renderText("Complete")
            }})
})
