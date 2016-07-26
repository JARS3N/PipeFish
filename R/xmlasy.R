Collect<-function(X){
  require(XML)
  require(dplyr)
  list(
    CAL = CalData(X),
    LVL = get_lvls(X),
    PH_COEF=pH_coefs(X),
    Inst = getInst(X),
    sn = getSn(X),
    Lot = getLot(X),
    assay=determineAssay(X),
    file=getFile(X)
  )
}

AnalyteIndex<-function(X){
  FND<-(xmlChildren(X[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]])) 
  positionAnalyte<-unlist(lapply(FND,function(u){xmlValue(u[["Key"]][["string"]])}))
  Analyte <-c(1,2) ;names(Analyte)<-positionAnalyte;
  Analyte
}
## Adding because of Legacy
getFile<-function(Z){basename(Z[['doc']]['file'][[1]])}
getInst<-function(Z){xmlValue(Z[["doc"]][[1]][["InstrumentSerialNumber"]])}


getSn<-function(u){
  sn <- xmlValue(u[["doc"]][[1]][["Cartridge"]][["Serial"]])
  x<-basename(u[['doc']]['file'][[1]])
  regexp<-"_[0-9]{1,3}_"
  if(length(sn)==0){
    sn<- unlist(regmatches(x, gregexpr(regexp, x))) 
    sn<-gsub("_","",sn)
    return(sn)
  }else{
    return(sn)}
}

getLot<-function(X){
  Lot<-paste0(xmlValue(X[["doc"]][[1]][["Cartridge"]][["Type"]]),
              xmlValue(X[["doc"]][[1]][["Cartridge"]][["Lot"]]))
  if (length(Lot)==0){
    regexp<-"[W,B,C,Q,T]{1}[E,0-9]{1}[0-9]{4}"
    Lot<- unlist(regmatches(getFile(X), gregexpr(regexp, x)))
  }
  return(Lot)
}

getCtgTypefromInst<-function(Z){
  Instperfix<-substr(getInst(Z),1,2)  
  lettr<-c("Q"=10,"W"=20,"C"=43,"W"=41,"B"=42)
  names(lettr[lettr==Instperfix])
}

getCtgType<-function(Z){
 Ltr<- xmlValue(Z[["doc"]][[1]][["Cartridge"]][["Type"]])
  if (length(Ltr)==0){
  Ltr<-getCtgTypefromInst(Z)
  }
 Ltr
}


########################
pH_coefs<-function(X){
  Analyte<-AnalyteIndex(X)
  coefs<-list(
    "slope"= as.numeric(xmlValue(X[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]][ Analyte["pH"]][["Item"]][2][["Value"]][["AnalyteCalibration"]][["GainEquation"]][["C3"]])
    ),
    "intercept"=as.numeric(xmlValue(X[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]][Analyte["pH"]][["Item"]][2][["Value"]][["AnalyteCalibration"]][["GainEquation"]][["C4"]])
    ),
    "target"=as.numeric(xmlValue(X[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]][Analyte["pH"]][["Item"]][2][["Value"]][["AnalyteCalibration"]][["TargetEmissionValue"]])
    ))
  coefs$gain<-c((coefs$slope * coefs$target)+ coefs$intercept);
  coefs
}


CalData<-function(u){
  A<- u[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]]
  merge(
    unlisted(A[[1]][["Value"]][["AnalyteCalibration"]]),
    unlisted(A[[2]][["Value"]][["AnalyteCalibration"]]),
    by='Well')
}



unlisted<-function(U){
  data.frame(
    LED= lapply(xmlChildren(U[["LedValues"]]),function(j){as.numeric(xmlSApply(j,xmlValue))}) %>%
      Reduce(f='c',x=.),
    CalEmission=lapply(xmlChildren(U[["CalibrationEmissionValues"]]),function(j){as.numeric(xmlSApply(j,xmlValue))}) %>%
      Reduce(f='c',x=.),
    Status=lapply(xmlChildren(U[["LedStatusValues"]]),function(j){(xmlSApply(j,xmlValue))}) %>%
      Reduce(f='c',x=.)) %>%
    setNames(paste0(xmlValue(U[["AnalyteName"]]),".",names(.))) %>%
    mutate(Well=1:length(.[,1]))
}

determineAssay<-function(X){
  basename(xmlValue(X[['doc']][[1]][['FileName']])) %>%
    c("Gain"=grepl("GAIN",toupper(.)),"Ksv"=grepl("KSV",toupper(.)))%>%
    (function(u){names(u[u==TRUE])})
}


get_lvls<-function(u){
  index<-c(1,2)
  AnalyteLoc<-u[['doc']][[1]][["AssayDataSet"]][["PlateTickDataSets"]][[1]][["AnalyteDataSetsByAnalyteName"]]
  nms<-c(xmlValue(AnalyteLoc[[1]][['Key']][['string']][['text']]),xmlValue(AnalyteLoc[[2]][['Key']][['string']][['text']]))
  names(index)<-nms
  Q<-u[['doc']][[1]][["AssayDataSet"]][["PlateTickDataSets"]]
  Lst<-lapply(seq_along(Q),function(j){
    data.frame(
      pHlvl=as.numeric(xmlSApply(Q[[j]][["AnalyteDataSetsByAnalyteName"]][index["pH"]][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]],xmlValue)),
      O2lvl=as.numeric(xmlSApply(Q[[j]][["AnalyteDataSetsByAnalyteName"]][index["O2"]][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]],xmlValue)),
      Tick=j-1,
      Well=factor(1:length(Q[[j]][["AnalyteDataSetsByAnalyteName"]][1][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]])))
  })
  merge(bind_rows(Lst),TickTable(u),by='Tick')
}



TickTable<-function(u){
  Q<-u[['doc']][[1]][["AssayDataSet"]][["RateSpans"]]
  bind_rows(lapply(1:length(Q),function(i){
    qfrom=as.numeric(xmlValue(Q[[i]][["StartTickIndex"]]));
    qto=as.numeric(xmlValue(Q[[i]][["EndTickIndex"]]));
    Measure<-i;
    data.frame(Tick=seq(from=qfrom,to=qto,by=1),Measure=Measure)})
  )
}



ComboAssay<-function(X){
  require(tidyr)
  Dat<-  list(
    pHgain=filter(X$LVL,Measure==1)%>%
      filter(.,Tick %in% c(max(Tick)-2,max(Tick)-1,max(Tick))) %>%
      group_by(.,Well) %>% summarise(.,sorpH=mean(pHlvl)) %>%
      merge(.,X$CAL,by='Well') %>%mutate(.,Target=X$PH_COEF$target) %>%
      mutate(.,Gain=(Target/pH.CalEmission)*(1/800)*(pH.CalEmission-sorpH)),
    O2gain=group_by(X$LVL,Measure) %>%
      filter(.,Tick %in% c(max(Tick)-2,max(Tick)-1,max(Tick))) %>%
      ungroup(.) %>%
      group_by(.,Well,Measure) %>% summarise(.,avgO2lvl=mean(O2lvl)) %>%
      mutate(.,Measure=c("Ambient","F0")[Measure])%>%
      spread(.,Measure,avgO2lvl) %>%
      mutate(.,KSV=((F0/Ambient)-1)/152) %>%
      ungroup(.) %>%
      mutate(Well=as.numeric(Well))
  )
  merge(Dat$pHgain,Dat$O2gain,by='Well') %>%
    mutate(.,inst=as.character(X$Inst))%>%
    mutate(.,sn=as.character(X$sn)) %>%
    mutate(.,Lot=as.character(X$Lot))
}



newGain<-function(X){
  filter(X$LVL,Tick %in% c(max(Tick)-2,max(Tick)-1,max(Tick))) %>%
    group_by(.,Well) %>% summarise(.,sorpH=mean(pHlvl)) %>%
    merge(.,X$CAL,by='Well') %>%mutate(.,Target=X$PH_COEF$target) %>%
    mutate(.,Gain=(Target/pH.CalEmission)*(1/800)*(pH.CalEmission-sorpH))
}

Ksv<-function(X){
  group_by(X$LVL,Measure) %>%
    filter(.,Tick %in% c(max(Tick)-2,max(Tick)-1,max(Tick))) %>%
    ungroup(.) %>%
    group_by(.,Well,Measure) %>% summarise(.,avgO2lvl=mean(O2lvl)) %>%
    mutate(.,Measure=c("Ambient","F0")[Measure])%>%
    tidyr::spread(.,Measure,avgO2lvl) %>%
    mutate(.,KSV=((F0/Ambient)-1)/152) %>%
    merge(.,X$CAL,by='Well')
}



assay<-function(X){
  AL<-list("Gain"=PipeFish::newGain,"Ksv"=PipeFish::Ksv)
  AL[[X$assay]](X) %>%
    mutate(sn=X$sn,Inst=X$Inst,Lot=X$Lot)
}

