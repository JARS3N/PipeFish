### look for sns of lot in DB
checkforDBPresence<-function(u){
  dcon<-PipeFish::dplyrCON()
  inDB<-dcon %>%
    tbl("machinevisiondata") %>%
    filter(Lot==u) %>%
    select(.,Lot,sn) %>%
    distinct() %>% collect()
  rm(dcon)
  gc()
  return(inDB)
}
###### write data.frame to database
writeDFtoDB <-function(DATA){
  con <-PipeFish::rmysqlCon()
  dbWriteTable(con, name="machinevisiondata",value= DATA,
               append=TRUE,overwrite = FALSE,row.names=FALSE)
  message("wrote to table")
  dbDisconnect(con)
  message("disconnect")
}

##### APP BELOW

library(RMySQL)
library(dplyr)
shinyServer(function(input, output,session) {
  output$status<-renderText('Waiting')
  output$status2<-renderText('')
  output$status3<-renderText('')
  output$status4<-renderText('')

  observeEvent(input$SelectDir,{
    output$status<-renderText('...working')
    DATA <- PipeFish::getDetailsXML()
    message("pulled data from details.xml files")
    inDat <-DATA %>% select(Lot) %>% distinct %>% {.$Lot}
    status2<-paste0("checking for ",inDat  ," and related sn in database")
    output$status2<-renderText({status2})
    message(status2)
    inDB<- checkforDBPresence(inDat)
    if(nrow(inDB)==0){
      status3<- paste0(inDat," new to database")
      output$status3<-renderText(status3)
      message(status3)
      writeDFtoDB(DATA)
    }else{
      sorted<- DATA %>% select(Lot=Lot,sn) %>% distinct()
      newCtgs<-anti_join(sorted,inDB)
      if(nrow(newCtgs)>0){
        finalData<-semi_join(DATA,newCtgs,by=c("Lot","sn"))
        writeDFtoDB(finalData)
        status4<-paste0(inDat," completed to database")
        output$status4<-renderText("Nothing new to Upload")
      }else{
        output$status4<-renderText("Nothing new to Upload")
        message('Nothing new to Upload')
      }
    }

    output$status5<-renderText("Task Complete\n ready for next Lot")
    message("Task Complete")
    Sys.sleep(3)
  })#end obsereEvent
})
