gradeOL<-function(u){c("A","B","C","D")[c(u < 5,u >=5 & u <10,u >=10 & u <20,u >= 20)]}

OLgrb<-function(u){
 #Import sheets from XLSX file
X<-list(LVL=readxl::read_excel(u,sheet='Level') ,
 AC=readxl::read_excel(u,sheet='Assay Configuration'))
# O2 Outliers
 O2 <-select(X$LVL,O2=contains("O2 (mmHg)" ),Well) %>%
       mutate(.,O2dif=abs(O2-152))  %>%
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
dplyr::rbind_all()
}
