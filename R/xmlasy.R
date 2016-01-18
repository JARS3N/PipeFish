library(XML)
library(dplyr)
library(tidyr)


Collect<-function(X){
require(XML)
    list(
        CAL = CalData(X),
        LVL = get_lvls(X),
        PH_COEF=pH_coefs(X),
        Inst = X$InstrumentSerialNumber,
        sn = X$Cartridge$Serial,
        Lot = paste0(X$Cartridge$Type,X$Cartridge$Lot),
        assay=determineAssay(X)
    )
}

CalData<-function(u){
    merge(
        unlisted(u$AssayDataSet$AnalyteCalibrationsByAnalyteName[[1]]$Value$AnalyteCalibration),
        unlisted(u$AssayDataSet$AnalyteCalibrationsByAnalyteName[[2]]$Value$AnalyteCalibration),
        by='Well')
}

unlisted<-function(U){
    DF<-data.frame(
        LED= as.numeric(unlist(U$LedValues)),
        CalEmission=as.numeric(unlist(U$CalibrationEmissionValues)),
        Status=factor(unlist(U$LedStatusValues)))
    names(DF)<-sapply(names(DF),function(j){paste0(U$AnalyteName,".",j)});
    DF$Well<-1:length(DF[,1]);DF
}


get_lvls<-function(u){
    Lst<-lapply(1:length(u$AssayDataSet$PlateTickDataSets),function(j){
        data.frame(
            pHlvl=as.numeric(unlist(u$AssayDataSet$PlateTickDataSets[[j]]$AnalyteDataSetsByAnalyteName[2]$Item$Value$AnalyteDataSet$CorrectedEmissionValues)),
            O2lvl=as.numeric(unlist(u$AssayDataSet$PlateTickDataSets[[j]]$AnalyteDataSetsByAnalyteName[1]$Item$Value$AnalyteDataSet$CorrectedEmissionValues)),
            Tick=j-1,
            Well=factor(1:length(unlist(u$AssayDataSet$PlateTickDataSets[[j]]$AnalyteDataSetsByAnalyteName[2]$Item$Value$AnalyteDataSet$CorrectedEmissionValues))))
    })
    merge(Reduce('rbind',Lst),TickTable(u),by='Tick')
}

pH_coefs<-function(u){coefs<-list(
    "slope"= as.numeric(u$AssayDataSet$AnalyteCalibrationsByAnalyteName[2]$Item[2]$Value$AnalyteCalibration$GainEquation$C3),
    "intercept"=as.numeric(u$AssayDataSet$AnalyteCalibrationsByAnalyteName[2]$Item[2]$Value$AnalyteCalibration$GainEquation$C4),
    "target"=as.numeric(u$AssayDataSet$AnalyteCalibrationsByAnalyteName[2]$Item[2]$Value$AnalyteCalibration$TargetEmissionValue))
coefs$gain<-c((coefs$slope * coefs$target)+ coefs$intercept);
coefs
}


determineAssay<-function(X){
    basename(X$FileName) %>%
    c("Gain"=grepl("GAIN",toupper(.)),"Ksv"=grepl("KSV",toupper(.)))%>%
        (function(u){names(u[u==TRUE])})
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
        spread(.,Measure,avgO2lvl) %>%
        mutate(.,KSV=((F0/Ambient)-1)/152) %>%
        merge(.,X$CAL,by='Well')
}

ComboAssay<-function(X){
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
        mutate(Well=as.numeric(Well))
    ) 
   merge(Dat$pHgain,Dat$O2gain,by='Well') %>%
        mutate(.,inst=as.character(X$Inst))%>%
        mutate(.,sn=as.character(X$sn)) %>%
        mutate(.,Lot=as.character(X$Lot))
}

TickTable<-function(u){
    Reduce('rbind',
           lapply(1:length(u$AssayDataSet$RateSpans),function(i){
               qfrom=as.numeric(u$AssayDataSet$RateSpans[[i]]$StartTickIndex);
               qto=as.numeric(u$AssayDataSet$RateSpans[[i]]$EndTickIndex);
               Measure<-i;
               data.frame(Tick=seq(from=qfrom,to=qto,by=1),Measure=Measure)})
    )
}
