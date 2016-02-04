Collect<-function(X){
    require(XML)
    require(dplyr)
    list(
        CAL = CalData(X),
        LVL = get_lvls(X),
        PH_COEF=pH_coefs(X),
        Inst = xmlValue(X[["doc"]][[1]][["InstrumentSerialNumber"]]),
        sn = xmlValue(X[["doc"]][[1]][["Cartridge"]][["Serial"]]),
        Lot = paste0(xmlValue(X[["doc"]][[1]][["Cartridge"]][["Type"]]),
                     xmlValue(X[["doc"]][[1]][["Cartridge"]][["Lot"]])),
        assay=determineAssay(X)
    )
}

pH_coefs<-function(X){
    FND<-(xmlChildren(X[["doc"]][[1]][["AssayDataSet"]][["AnalyteCalibrationsByAnalyteName"]])) 
    positionAnalyte<-unlist(lapply(FND,function(u){xmlValue(u[["Key"]][["string"]])}))
    Analyte <-c(1,2) ;names(Analyte)<-positionAnalyte
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
    Q<-u[['doc']][[1]][["AssayDataSet"]][["PlateTickDataSets"]]
    Lst<-lapply(1:length(Q),function(j){
        data.frame(
            pHlvl=as.numeric(xmlSApply(Q[[j]][["AnalyteDataSetsByAnalyteName"]][2][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]],xmlValue)),
            O2lvl=as.numeric(xmlSApply(Q[[j]][["AnalyteDataSetsByAnalyteName"]][1][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]],xmlValue)),
            Tick=j-1,
            Well=factor(1:length(Q[[j]][["AnalyteDataSetsByAnalyteName"]][1][["Item"]][["Value"]][["AnalyteDataSet"]][["CorrectedEmissionValues"]])))
    })
    merge(rbind_all(Lst),TickTable(u),by='Tick')
}



TickTable<-function(u){
 Q<-u[['doc']][[1]][["AssayDataSet"]][["RateSpans"]]
    rbind_all(lapply(1:length(Q),function(i){
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
        spread(.,Measure,avgO2lvl) %>%
        mutate(.,KSV=((F0/Ambient)-1)/152) %>%
        merge(.,X$CAL,by='Well')
}
