##
## Read in whole excel sheet

    
    WB<-function(u){
    require(dplyr)
    #get sheet names
    readxl::excel_sheets(u) %>%
    #split names into list to retain names
  split(.,.) %>% 
  #lapply read all sheets into corresponding sheetnames in list
  lapply(.,function(k){
    readxl::read_excel(u)
    })
    }
