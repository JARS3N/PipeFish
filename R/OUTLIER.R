gradeOL<-Vectorize(function(u){c("A","B","C","D")[c(u < 5,u >=5 & u <10,u >=10 & u <20,u >= 20)]})

gradepH<-Vectorize(function(u){
c("A", "B", "C", "D")[c(u < 0.1, u >= 0.1 & u <= 0.2, u > 0.2 & u <= 0.4, u > 0.4)]
})

OLgrb<-function(u){
 #Import sheets from XLSX file
X<-list(LVL=readxl::read_excel(u,sheet=PipeFish::fndLVLs(u)) ,
 AC=readxl::read_excel(u,sheet='Assay Configuration'))
# O2 Outliers
 O2 <-select(X$LVL,O2=contains("O2 (mmHg)" ),Well) %>%
       mutate(.,O2dif=abs(O2-152))  %>%
      group_by(.,Well) %>%
      filter(.,O2dif==max(O2dif)) %>%
     slice(.,1) %>%
       ungroup(.) %>%
        rename(.,mxO2=O2dif) %>%
       mutate(.,grade=gradeOL(mxO2))
    #pH Outliers
pH<-select(X$LVL,Well,pH) %>%
mutate(.,pHdif=abs(pH-7.4))  %>%
group_by(.,Well) %>%
 filter(.,pHdif==max(pHdif)) %>%
slice(.,1) %>%
ungroup(.)%>%
mutate(.,gradepH=gradepH(pHdif))

                   # Tick zero median

                        T0 <-  select(X$LVL,O2=contains("O2 (mmHg)" ),Well,Tick) %>%
                                filter(.,Tick==min(Tick)) %>%
                                summarize(med=median(O2))
                        # Final merged output
                       Meta <-setNames(unlist(X$AC[,2]),unlist(X$AC[,1]))
 
                        MedianFirstTick = T0$med
                             metainfo <-data.frame(
                            sn=Meta["Cartridge Serial"],
                            Lot=Meta["Cartridge Lot"] ,
                            Instrument=Meta["Instrument Serial"],
                            fl=u,
                            MedianFirstTick = T0$med
                            )

                            merge(O2,pH,by='Well') %>%
                              mutate(.,fl=u) %>%
                              merge(.,metainfo,by='fl')
                    }
### since I'm very lazy and seem to reproduce this piece all the time:
OLgrbs<-function(PATH=getwd()){
require(dplyr)
list.files(pattern='.xlsx',path=PATH,full.names=TRUE) %>%
grep(pattern="~",x=.,invert=T,value=T)  %>%
lapply(.,PipeFish::OLgrb) %>%
dplyr::bind_rows()
}

plotOLCTG<-function(df,nameexp){
    require(ggplot2);require(ggthemes);require(dplyr)
    nRuns<-(length(unique(df$fl)))
    df %>% mutate(ctg=paste0(Lot,"_",sn)) %>%
        ggplot(aes(grade)) +
        geom_bar(aes(fill=grade))+
        facet_wrap(~ctg)+
        ggtitle(paste0(nameexp,",n=",nRuns)) +
        theme_bw()+
        scale_fill_tableau()
}


plotOLAVGS <-function(df,nameexp){
    require(ggplot2);require(ggthemes);require(dplyr)
    nWells<-length(unique(df$Well))
    nRuns<-(length(unique(df$fl)))
   df%>%
    group_by(.,grade) %>%
    summarize(.,peraverage=round(n()/(nRuns* nWells)*100,3)) %>%
    ggplot(.,aes(grade,peraverage)) +
       geom_bar(aes(fill=grade),stat='identity')+
       theme_bw()+
       scale_fill_tableau()+
       geom_text(aes(label =paste0(round(peraverage,3),"%"),
                     x = grade, y = peraverage),
                 position = position_dodge(width = 0.8), vjust = -0.6)+
       ggtitle(paste0(nameexp,",n=",nRuns))
}


TempOLgrb<-function(u){
  #Import sheets from XLSX file
  X<-list(LVL=readxl::read_excel(u,sheet=PipeFish::fndLVLs(u)) ,
          AC=readxl::read_excel(u,sheet='Assay Configuration'))
  # O2 Outliers
  O2 <-select(X$LVL,O2=contains("O2 (mmHg)" ),Well,Temp=`Well Temperature`) %>%
    mutate(.,O2dif=abs(O2-PipeFish::pO2(Temp)))  %>%
    group_by(.,Well) %>%
    filter(.,O2dif==max(O2dif)) %>%
    slice(.,1) %>%
    ungroup(.) %>%
    rename(.,mxO2=O2dif) %>%
    mutate(.,grade=sapply(mxO2,gradeOL))
  #pH Outliers
  pH<-select(X$LVL,Well,pH) %>%
    mutate(.,pHdif=abs(pH-7.4))  %>%
    group_by(.,Well) %>%
    filter(.,pHdif==max(pHdif)) %>%
    slice(.,1) %>%
    ungroup(.)
  
  # Tick zero median
  
  T0 =  select(X$LVL,O2=contains("O2 (mmHg)" ),Well,Tick) %>%
    filter(.,Tick==min(Tick)) %>%
    summarize(med=median(O2))
  # Final merged output
  
  sn=unlist(X$AC[which(X$AC[,1]=="Cartridge Serial"),2])
  Lot=unlist(X$AC[which(X$AC[,1]=="Cartridge Lot"),2])
  Instrument=unlist(X$AC[which(X$AC[,1]=="Instrument Serial"),2])
  fl=u
  MedianFirstTick = T0$med
  metainfo <-data.frame(
    sn=sn,
    Lot=Lot ,
    Instrument=Instrument,
    fl=fl,
    MedianFirstTick = MedianFirstTick
  )
  
  merge(O2,pH,by='Well') %>%
    mutate(.,fl=u) %>%
    merge(.,metainfo,by='fl')
}
### since I'm very lazy and seem to reproduce this piece all the time:
OLgrbs<-function(PATH=getwd()){
  require(dplyr)
  list.files(pattern='.xlsx',path=PATH,full.names=TRUE) %>%
    grep(pattern="~",x=.,invert=T,value=T)  %>%
    lapply(.,PipeFish::OLgrb) %>%
    dplyr::bind_rows()
}

plotOLCTG<-function(df,nameexp){
  require(ggplot2);require(ggthemes);require(dplyr)
  nRuns<-(length(unique(df$fl)))
  df %>% mutate(ctg=paste0(Lot,"_",sn)) %>%
    ggplot(aes(grade)) +
    geom_bar(aes(fill=grade))+
    facet_wrap(~ctg)+
    ggtitle(paste0(nameexp,",n=",nRuns)) +
    theme_bw()+
    scale_fill_tableau()
}




