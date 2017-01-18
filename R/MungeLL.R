
mungeLL<-function(xlfile){
require(readxl)
require(dplyr)
require(tidyr)


#Pulls the Data from column 2 corresponding to the identifiers in column 1
getIT<-function(.data,term,y1=1,y2=2){
  .data[which(.data[,y1]==term),y2]
}

#Pull all the Meta data from the top of the excel sheet
pullMeta<-function(XL,xlfile){
  c("Light Leak Test","Serial number:","Date:",
    "Time Start:","Time End:","Final result:",
    "High Limit Multiplicator:","Low Limit Multiplicator:",
    "Num Points:"
  ) %>%
    assign("nms",.,envir=parent.env(environment())) %>%
    lapply(.,{. %>% getIT(XL,.)}) %>%
    unlist %>%
    data.frame(values=.) %>%
    mutate(nms=gsub("[ ]|[:]","",nms)) %>%
    tidyr::spread(.,nms,values) %>%
    mutate(.,file=basename(xlfile))

}
# find the analytes
indexAnalyte<-function(XL){data.frame(i=which(XL[,1]=="Analyte:"),
              a=getIT(XL,"Analyte:")
)
}
# split data on analytes
splitAnalytes<-function(XL,w){
  list(A = XL %>% slice(.,(w$i[1]-1):(w$i[2]-1)),
       B = XL %>% slice(.,(w$i[2]-1):(nrow(XL)))
  ) %>%
    setNames(.,w$a)
}
# Using Meta Data sbset for correct Number of columns
# because data format from Utility is inconsistent
nofCol<-function(Meta){
  x<-substr(Meta$Serialnumber,1,2)
  list("42"=6,"41"=12,"43"=1,"21"=12)[x]
}
# Finally Pull the data from each table
getTable<-function(df,thing){
  CAL<-which(df==thing)
  whereisA<-which(df[,1]=="A") %>%.[. > CAL ] %>% .[1]
  whereispD<-which(df[,1]=="% Diff") %>% .[. > CAL ] %>% .[1]
  df %>%
    slice(.,(whereisA-1):whereispD) %>%
    mutate(nchar= nchar(X0)==1) %>%
    filter(.,nchar==T) %>%
    select(-nchar) %>%
    tidyr::gather(.,X0) %>%
    magrittr::set_names(c("Let","Col","Val")) %>%
    mutate(Col=gsub("X","",Col) %>% as.numeric()) %>%
    filter(.,Col<=as.numeric(nofCol(Meta)))%>%
    mutate(.,Col=sprintf("%02d",Col)) %>%
    tidyr::unite(.,Well,Let,Col,sep="") %>%
    mutate(.,Val=as.numeric(Val))
}



XL<-readxl::read_excel(xlfile,sheet= 'Results',col_names = F) %>% as.data.frame()
Meta<-pullMeta(XL,xlfile)
w<-indexAnalyte(XL)
data<-splitAnalytes(XL,indexAnalyte(XL))

list(
getTable(data$O2,"Emissions") %>%
  rename(O2LL=Val),
getTable(data$O2,"Calibration LED") %>%
  rename(O2LED=Val),
getTable(data$pH,"Emissions") %>%
  rename(pHLL=Val),
getTable(data$pH,"Calibration LED") %>%
  rename(pHLED=Val)
)%>%
  Reduce('inner_join',.) %>%
  list(data = .,meta = Meta)

}


#get directory
getLightLeakFiles <-  function(dir,opensavexlsx=F){
  require(dplyr);
  if (!opensavexlsx == T) {
    PipeFish::XLSXos(dir)
  }
  list.files(dir,recursive = T,pattern = 'xlsx',full.names = T) %>%
    lapply(.,mungeLL)
  # needs to add a two part upload to SQL db
  # 1. add meta data to table and get index for key
  # 2. add key to data df and upload that to othertable


}