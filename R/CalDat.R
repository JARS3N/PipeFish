CalDat<-function(fl=file.choose()){
  require(dplyr)
  GetValues<-function(J){
    Dat<-lapply(seq_along(J),function(x){
      J[[x]] %>% XML::xmlSApply(.,XML::xmlValue)})  %>% unlist
    if(unique(names(Dat))=="CalibrationQuality"){
      return(Dat %>% as.character())
    }else{
      return(Dat %>% as.numeric())
    }
  }
  getType<-function(L){
    data.frame(n=c(8,24,96),ctype=c("C","B","W")) %>% 
      filter(.,n==L) %>% 
      select(ctype) %>% 
      .$ctype %>% as.vector()
  }
  getWells<-function(Type){
    list(
    "W"= paste0(sapply(LETTERS[1:8],rep,12),sprintf("%02d",c(1:12))),
    "B" = paste0(sapply(LETTERS[1:4],rep,6),sprintf("%02d",c(1:6))),
    "C" = paste0(LETTERS[1:8],"01")
    )[[Type]]
  }
  A<-XML::xmlTreeParse(fl)

 pullData<-function(dat){
    A<-dat$doc$children$CalibrationData
    analyte<-list("O2","PH")
    vars<-list("LED"="LedValues","Status"="LedStatusValues",
               "CalEmission"="CalibrationEmissionValues",
               "RefDelta"="IntialReferenceDeltaValues")
    perAnalyte<-function(An){
      lapply(vars,{. %>% A[[paste0(An,"Data")]][[.]] %>% GetValues(.)}) %>% 
        as.data.frame.list(.,col.names=paste(An,names(.),sep="."))
    }
    lapply(analyte,perAnalyte) %>% 
      bind_cols() %>% 
      mutate(.,type=getType(nrow(.))) %>% 
      mutate(Lot=paste0(type,XML::xmlValue(A[["CartridgeLot"]]))) %>% 
      mutate(sn=XML::xmlValue(A[["CartridgeSerial"]])) %>% 
      mutate(Well=getWells(unique(type))) %>% 
      select(.,-type)
 }
 pullData(A)
}
