getOLdata<-function(u){
    #u is directory
    require(PipeFish);require(dplyr);require(rio)
    list.files(pattern = "xlsx",full.names=T,path= u) %>%
        grep(pattern="~",invert=T,value=T)  %>%
        lapply(OLgrb) %>%
        rbind_all
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
