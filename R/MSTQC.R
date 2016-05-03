         FCCP<- function(u){
           require(dplyr)
           u %>%
             filter(.,conc == 0.5) %>%
             filter(.,Measurement == 7) %>%
             group_by(kit) %>%
             summarize(MeanOCR=mean(OCR)) %>%
             mutate(.,PercentControl=MeanOCR/(MeanOCR[kit=="control"])*100,Lot=unique(u$Lot)) %>%
             mutate(.,reagent="fccp")
         }

         Oligo<- function(u){
           require(dplyr)
           u %>%
             filter(.,conc == 1,Measurement == 4) %>%
             group_by(kit) %>%
             summarize(MeanOCR=mean(OCR)) %>%
             mutate(.,PercentControl=MeanOCR/(MeanOCR[kit=="control"])*100,Lot=unique(u$Lot))%>%
             mutate(.,reagent="oligo")
         }

         AntRot<- function(u){
           require(dplyr)
           require(tidyr)
           dplyr::filter(u,Measurement==9 | Measurement==10 ) %>%
             dplyr::filter(.,conc==0.5) %>%
             tidyr::spread(.,key=Measurement,value=OCR) %>%
             group_by(kit) %>%
             summarize(avgm9=mean(`9`),avgm10=mean(`10`),MeanOCR=avgm9-avgm10,Lot=unique(u$Lot)) %>%
             mutate(PercentControl=MeanOCR/(MeanOCR[kit=="control"])*100) %>%
             select(-avgm9,-avgm10) %>%
             mutate(.,reagent="antrot")
         }


         MST<-function(X){
           SORTS<-list("fccp"=FCCP,"oligo"=Oligo,"antrot"=AntRot);
           SORTS[[unique(X$Reagent)]](X)
         }



         fndLOT<-function(STRING){
           str<-gsub("[ ]+"," ",tolower(STRING))
          get<- paste0(unlist(regmatches(x=str,m=gregexpr("lot [0-9]+",str))),collapse="")
          as.numeric(gsub("lot","",get))
         }



         CleanMST <-function(y){
           require(dplyr)
           rio::import(y,sheet="Rate") %>%
             filter(.,Group!="Background") %>%
             select(.,-Time,-PPR,-ECAR) %>%
             mutate(.,Group=sapply(Group,tolower)) %>%
             mutate(.,Group=gsub("[ ]+","",Group)) %>%
             mutate(.,Reagent=sapply(Group,function(u){strsplit(u,split="[|]")[[1]][2]})) %>%
             mutate(.,kit=sapply(Group,function(u){strsplit(u,split="[|]")[[1]][3]})) %>%
             mutate(.,conc=sapply(Group,function(u){strsplit(u,split="[|]")[[1]][4]})) %>%
             mutate(.,conc=as.numeric(gsub("[a-z]+","",conc))) %>%
             filter(.,kit !="media") %>%
             filter(.,kit !="Ignore") %>%
             mutate(.,Reagent=gsub("[-,_]+","",Reagent))%>%
             mutate(Lot=fndLOT(y))
         }
