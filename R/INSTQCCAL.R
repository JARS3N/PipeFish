UploadINSTCAL<-function(DIR,recursiveIN = F){
  suppressMessages(require(RMySQL))
  suppressMessages(require(dplyr))
  suppressMessages(require(XML))
dbAddINSTCAL<-function(X){
  my_dbADD <- rmysqlCon()
  writeMeta<-dbWriteTable(my_dbADD, name="instqccalmeta",value=X$CalMeta,
                          append=TRUE,overwrite = FALSE,row.names=FALSE)
  if(writeMeta){
    nID<-getIDnMeta() 
    X$CalData %>% 
      merge(.,nID) %>% 
      DBI::dbWriteTable(my_dbADD, name="instqccaldata",value=.,
                   append=TRUE,overwrite = FALSE,row.names=FALSE)
    
    X$FailModes %>% 
      dplyr::select(.,-file) %>%
      merge(.,nID) %>% 
      DBI::dbWriteTable(my_dbADD, name="instqccalfail",value=.,
                   append=TRUE,overwrite = FALSE,row.names=FALSE)
    DBI::dbDisconnect(my_dbADD)
  }else{
    message("Failed to write Meta Table,ignoring rest of file")
  }
}


getIDnMeta<-function(){
  require(RMySQL)
  my_dbN <- rmysqlCon()
  n<-DBI::dbGetQuery(my_dbN,"SELECT ID FROM INSTQCCALMETA ORDER BY ID DESC LIMIT 1;")
  DBI::dbDisconnect(my_dbN)
  n
}

dbSendListToDB<-function(L){
  dbAddINSTCAL(L[[1]])
  L<-L[-1]
  if(length(L)>0){
    dbSendListToDB(L)
  }else(message("Completed"))
}


DIR %>% 
list.files(pattern='asyr',full.names=T,path=.,recursive=recursiveIN) %>% 
  grep("cancelled",invert=T,value=T,.) %>%
  lapply(., XML::xmlTreeParse) %>% 
  lapply(.,PipeFish::CollectNoLVL) %>% 
  lapply(.,{. %>% list(
    CalData=.$CAL,
    FailModes=CalibrationFailureModes(.),
    CalMeta=data.frame(
      file=.$file,
      Inst=.$Inst,
      Lot=.$Lot,
      sn=.$sn,
      tempC=.$calStartTemp,
      O2Target=.$O2_COEF$target,
      pHTarget=.$PH_COEF$target)
  )
  }) %>% 
  dbSendListToDB(.)

}
### App below
  
  UploadInstQCCalApp<-function(){
  suppressMessages(require(shiny))
  require(PipeFish)
  suppressMessages(require(miniUI))


ui <-miniPage( fluidPage(
  p("Select Directory with .asyr files to Upload "),mainPanel(
    actionButton("Run", "Select Directory"),
    checkboxInput("Recursive", "search recursively", FALSE),
    textOutput("session"),
    actionButton("DONE", "Exit"))

))

server <- function(input, output,session) {
  observeEvent(input$Run, {
    fldr<-choose.dir();
    #print(input$Recursive)
    if(!is.na(fldr)){
    PipeFish::UploadINSTCAL(fldr,input$Recursive)
      }
    rm(fldr)
  }) 

  observeEvent(input$DONE, {
    stopApp(returnValue = invisible())
  }) 
}

runGadget(ui, server,viewer = dialogViewer("Upload instrument QC Calibration Data", width = 400 ,height = 400))


}
