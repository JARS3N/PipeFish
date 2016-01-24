library(shiny)
library(RMySQL)
library(PipeFish)
library(dplyr)
library(XML)
ConnectInfo<-readRDS(file.path((.libPaths()),"DataStash","DBinfo.RDS"))

my_db <- dbConnect(RMySQL::MySQL(),
                   dbname=ConnectInfo["dbname"],
                   user=ConnectInfo["user"],
                   password=ConnectInfo["password"],
                   host=ConnectInfo["host"],
                   port=as.numeric(ConnectInfo["port"]))

shinyServer(function(input, output) {
    observe({
        if(input$goButton > 0){
            DIR<-choose.dir();
           DF<- DIR %>%
                list.files(path=.,pattern='asyr',full.names=TRUE) %>%
                lapply(.,XML::xmlTreeParse) %>%
                lapply(.,PipeFish::Collect) %>%
                lapply(.,PipeFish::ComboAssay) %>%
                rbind_all(.)

            dbWriteTable(my_db, name="xfpwetqc", value=DF,append=TRUE,
                         overwrite = FALSE,row.names=FALSE)
            dbDisconnect(my_db)



            }})
})
