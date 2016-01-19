gradeOL<-function(u){c("A","B","C","D")[c(u < 5,u >=5 & u <10,u >=10 & u <20,u >= 20)]}

OLgrb<-function(u){
    #Import sheets from XLSX file
    X<-list(LVL=import(file=u,sheet='Level') ,
            AC=import(u,sheet='Assay Configuration'))
    # O2 Outliers
    O2 <-select(X$LVL,O2=contains("O2 (mmHg)" ),Well) %>%
        mutate(.,O2dif=abs(O2-152))  %>%
        group_by(.,Well) %>%
        summarize(.,mxO2=max(O2dif)) %>%
        mutate(.,grade=sapply(mxO2,gradeOL))
    #pH Outliers
      pH <- select(X$LVL, Well, pH) %>%
        mutate(., pHdif = abs(pH -  7.4)) %>%
        group_by(., Well) %>% 
        summarise(., pHdif = max(pHdif)) 
    # Tick zero median

    T0 =  select(X$LVL,O2=contains("O2 (mmHg)" ),Well,Tick) %>%
        filter(.,Tick==min(Tick)) %>%
        summarize(med=median(O2))
    # Final merged output
    merge(O2,pH,by='Well') %>%
        mutate(.,Lot=X$AC[27,2]) %>%
        mutate(.,sn=X$AC[26,2]) %>%
        mutate(.,Instrument=X$AC[35,2])%>%
        mutate(.,fl=u)  %>%
        mutate(., MedianFirstTick = T0$med )
}
