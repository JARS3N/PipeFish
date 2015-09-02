tickfilter.A<-function(u){c(max(u),max(u)-1,max(u)-2)}
tickfilter.B<-function(u){c(floor(median(u))-3,floor(median(u))-2,floor(median(u))-1,max(u)-2,max(u)-1,max(u))}
##################
grab96e<-function(u){
#u is full file name#plat is platform type
  pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
    import(u,sheet="Level") %>%
    mutate(fl=u) %>%
    filter(Tick %in% tickfilter.A(Tick)) %>%
    select(counts=contains("pH Corrected Em."),Tick,Well,fl) %>%
    mutate(pH=pH[as.numeric(factor(Well))]) %>%
    (function(u){merge(u,data.frame(dye=c(rep('CL',6),rep('PR',6)),Well=unique(u$Well)),by="Well")}) %>%
    group_by(Well,pH,dye,fl) %>% summarise(counts=mean(counts))
}
#####################
grabXFp<-function(u){
  pH=c(3.8,5,5.8,6.6,7.0,7.4,8.15,9.2)
  import(u,sheet="Level") %>%
    mutate(fl=u) %>%
    select(counts=contains("pH Corrected Em."),Tick,Well,fl)%>%
    filter(Tick %in% tickfilter.B(Tick)) %>%
    mutate(Tick=as.numeric(factor(Tick))) %>%
    mutate(dye = c("CL","PR")[Tick],pH=pH[as.numeric(factor(Well))]) %>%
    group_by(Well,pH,dye,fl) %>% summarise(counts=mean(counts))
}

################################
grab24e<-function(u){
  pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
  import(u,sheet="Level") %>%
    mutate(fl=u) %>%
    select(counts=contains("pH Corrected Em."),Tick,Well,fl)%>%
    filter(Tick %in% tickfilter.B(Tick)) %>%
    mutate(Tick=as.numeric(factor(Tick))) %>%
    mutate(dye = c("CL","PR")[Tick],pH=pH[as.numeric(factor(Well))]) %>%
    group_by(Well,pH,dye,fl) %>% summarise(counts=mean(counts))
}
###############
 grabXF24<-function(u){
   pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
   import(u,sheet="Levels",startRow = 12,readxl = FALSE,skipEmptyRows=FALSE) %>%
    mutate(fl=u) %>%
   select(counts=contains("pH.Cor..Em." ),Tick,Well,fl) %>%
   filter(Tick %in% tickfilter.B(Tick)) %>%
   mutate(Tick=as.numeric(factor(Tick))) %>%
   mutate(dye = c(rep("CL",3),rep("PR",3))[Tick]) %>%
   mutate(pH=pH[as.numeric(factor(Well))]) %>%
   group_by(Well,pH,dye,fl) %>% summarise(counts=mean(counts))
 }
##################
mungelist<-list(grab24e,grab96e,grabXF24,grabXFp)
####function to run pKa
pKa<-function(pHFluor,MFBatch,Platform,Directory){
list.files(path=Directory,pattern='xlsx',full.names = TRUE)   %>%
lapply(mungelist[[Platform]]) %>% Reduce(f='rbind') %>%
write.csv(file=paste0(Directory,"/","data.csv"),row.names=F)
createRmd(pHFluor,MFBatch,Directory) %>%
write(file=paste0(Directory,"/",pHFluor,"pKa.Rmd"))
knit(paste0(Directory,"/",pHFluor,"pKa.Rmd"))
pandoc(paste0(Directory,"/",pHFluor,"pKa.md"),format="latex")
knit2html(paste0(Directory,"/",pHFluor,"pKa.md"))
}
createRmd<-function(pHFluor,MFBatch,Directory){
pKaRmd<-readLines(system.file("rmd/pKaTemplate.Rmd", package="pipefish"))
pKaRmd[15]<-gsub('data.csv',paste0("Directory","/","data.csv"),pKaRmd[15])
pKaRmd[6]<-gsub('XBATCHX',MFBatch,pKaRmd[6])
pKaRmd[4]<-gsub('XLOTX',pHFluor,pKaRmd[4])
pKaRmd[43]<-gsub('pKA',paste0('pKa for ',pHFluor),pKaRmd[43])
}




