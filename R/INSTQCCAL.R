UploadINSTCAL<-function(DIR,recursiveIN = F){
  suppressMessages(require(RMySQL))
  suppressMessages(require(dplyr))
  suppressMessages(require(XML))
dbAddINSTCAL<-function(X){
  
  ConnectInfo<-DataStash::Triton()
  my_dbADD <- DBI::dbConnect(RMySQL::MySQL(),
                     dbname=ConnectInfo[1],user=ConnectInfo[2],
                     password=ConnectInfo[3],host=ConnectInfo[4],
                     port=as.numeric(ConnectInfo[5]))
  writeMeta<-dbWriteTable(my_dbADD, name="instqccalmeta",value=X$CalMeta,
                          append=TRUE,overwrite = FALSE,row.names=FALSE)
  if(writeMeta){
    nID<-getIDnMeta() 
    X$CalData %>% 
      merge(.,nID) %>% 
      DBI::dbWriteTable(my_dbADD, name="instqccaldata",value=.,
                   append=TRUE,overwrite = FALSE,row.names=FALSE)
    
    X$FailModes %>% 
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
  ConnectInfo<-DataStash::Triton()
  my_dbN <- DBI::dbConnect(RMySQL::MySQL(),
                     dbname=ConnectInfo[1],
                     user=ConnectInfo[2],
                     password=ConnectInfo[3],
                     host=ConnectInfo[4],
                     port=as.numeric(ConnectInfo[5]))
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