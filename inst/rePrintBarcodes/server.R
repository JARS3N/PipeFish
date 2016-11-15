library(shiny)
library(dplyr)
 ConnectInfo<-DataStash::Triton()
  my_db <- src_mysql(ConnectInfo[1],
                     user=ConnectInfo[2],
                     password=ConnectInfo[3],
                     host=ConnectInfo[4],
                     port=as.numeric(ConnectInfo[5]))
Q<-my_db %>%
  tbl('barcodelotview') %>% 
  select(Lot_Num) %>% 
  distinct() %>% 
  collect() %>% 
  .$Lot_Num
shinyServer(function(input, output,session) {
  updateSelectInput(session,'Lot',choices=c("N/A",Q))
  
  observeEvent(input$Lot,{
    if(input$Lot!="N/A"){
     lotn<- substr(input$Lot,2,nchar(input$Lot))
      lotl<-substr(input$Lot,1,1)
  DF<-  my_db %>%
      tbl('barcodelotview') %>%
    filter(Lot_Num_Input==lotn) %>%
   filter(Cart_type==lotl) %>%
    collect() %>% 
    list('Matrix'=.)
  dir<-"//lexnas03a-vs1-nas-s1lkq.agilent.com/LSAG/Seahorse Bioscience Chicopee/SH Consumables Labeling"
  outfl<-'CartridgeDatabase.xlsx'
  library(openxlsx)
  write.xlsx(DF,file=file.path(dir,outfl))

    }
  })
  
  
  
})
