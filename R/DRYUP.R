UploadDRYQC <- function() {
  require(shiny)
  require(PipeFish)
  require(miniUI)
  library(RMySQL)
  
  
  ui<-miniPage(fluidPage(
    p("Add QCStats from DryQC to Database"),mainPanel(
      actionButton("EXP", "EnterData"),
      textOutput("session"),
      actionButton("DONE", "DONE"))
  ))
  
  server<-function(input,output,session) {
    
    stuffHappens<-  observeEvent(input$EXP,
                                 {fl<-choose.files(multi=FALSE)
                                 if(grepl("xls",fl)){
                                   DF<-loadQCstats(fl)
                                   if(nrow(DF)>1){
                                     ConnectInfo<-DataStash::Triton()
                                     my_db <- dbConnect(RMySQL::MySQL(),
                                                                            dbname=ConnectInfo[1],
                                                                            user=ConnectInfo[2],
                                                                            password=ConnectInfo[3],
                                                                            host=ConnectInfo[4],
                                                                            port=as.numeric(ConnectInfo[5]))
                                     dbWriteTable(my_db, name="dryqcxf24",value=DF,
                                                  append=TRUE,overwrite = FALSE,row.names=FALSE)
                                     dbDisconnect(my_db)
                                     output$session <- renderText("Script Complete,Data Uploaded")
                                   }else{
                                     output$session <- renderText("Error:No Data Added")
                                   }
                                   
                                 }else{
                                   output$session <- renderText("Needs to be an Excel File.")
                                 }
                                 
                                 
                                 })
    observeEvent(input$DONE, {
      stopApp(returnValue = invisible())
    })
  }
  runGadget(ui, server,viewer = dialogViewer("UploadDRYQC", width = 400 ,height = 400))
}

filterLastRun<-function(U){
  ctg<-unique(U$Lot)  
  dates<-unique(U$date)
  if(length(dates)==1){
    return(U)
  }else{
    datevalues<-sapply(dates,function(u){as.POSIXct(u,format="%m/%d/%y %H:%M:%S")}) 
    last<-dates[which(datevalues==max(datevalues))]
    U<-filter(U,date==last)
    return(U)
  }
}

loadQCstats<-function(x){
 require(readxl)
  require(dplyr)
  require(tidyr)
  DryQCINST<-readxl::read_excel(x,sheet='File List') %>% 
    .[1,1] %>% grepl("DRY QC#2",.) %>% as.numeric()+1
  readxl::read_excel(x,sheet='Data') %>% 
    filter(.,`Cartridge Lot` != "Average" ) %>% 
    filter(., `Cartridge Lot` !=  "Std.Dev") %>% 
    select(.,-Operator,-`Num Failure O2`,-`Num Failure pH`,-`Test Result`) %>% 
    gather(.,key='sensewell',value='transmission',-`Cartridge Lot`,
           -`Cartridge Serial`, -`Cartridge Type`  , -`Test Date` , 
           -`O2 Threshold %`  , -`pH Threshold %`   
    ) %>% 
    mutate(.,well=gsub(' pH| O2',"",sensewell)) %>% 
    mutate(.,analyte=gsub("[A-Z]{1}[0-9]{1} ","",sensewell)) %>% 
    select(.,-sensewell) %>% 
    tidyr::spread(.,key=analyte,value=transmission) %>% 
    rename(.,Lot=`Cartridge Lot`,
           sn=`Cartridge Serial`,
           type =`Cartridge Type`,
           date = `Test Date`,
           O2Threshold =`O2 Threshold %`,
           pHThreshold =`pH Threshold %`
    ) %>% 
    mutate(.,Lot=paste0(type,Lot)) %>% 
    select(.,-type) %>% 
    mutate(ctg=paste0(Lot,"_",sn)) %>% 
    mutate(dryQC=DryQCINST) %>% 
    split(.,.$ctg) %>% 
    lapply(.,filterLastRun) %>%
    bind_rows() %>% 
    select(.,-ctg)
}
