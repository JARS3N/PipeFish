tickfilter.A<-function(u){c(max(u),max(u)-1,max(u)-2)}
tickfilter.B<-function(u){c(floor(median(u))-3,floor(median(u))-2,floor(median(u))-1,max(u)-2,max(u)-1,max(u))}
##################
grab96e<-function(u){
  #u is full file name#plat is platform type
  pH=c(rep(3.8,12),rep(5,12),rep(5.8,12),rep(6.6,12),rep(7.0,12),rep(7.4,12),rep(8.15,12),rep(9.2,12))
  readxl::read_excel(u,sheet="Level") %>%
    dplyr::mutate(.,fl=u) %>%
   dplyr::filter(.,Tick %in% tickfilter.A(Tick)) %>%
   dplyr::select(.,counts=contains("pH Corrected Em."),Tick,Well,fl) %>%
    dplyr::mutate(.,pH=pH[as.numeric(factor(Well))]) %>%
    merge(.,data.frame(dye=c(rep('CL',6),rep('PR',6)),Well=unique(u$Well)),by="Well") %>%
    dplyr::group_by(.,Well,pH,dye,fl) %>% 
    dplyr::summarise(.,counts=mean(counts))
}
#####################
grabXFp<-function(u){
  pH=c(3.8,5,5.8,6.6,7.0,7.4,8.15,9.2)
  readxl::read_excel(u,sheet="Level") %>%
    dplyr::mutate(.,fl=u) %>%
   dplyr::select(.,counts=contains("pH Corrected Em."),Tick,Well,fl)%>%
   dplyr::filter(.,Tick %in% tickfilter.B(Tick)) %>%
    dplyr::mutate(.,Tick=as.numeric(factor(Tick))) %>%
    dplyr::mutate(.,dye = c("CL","PR")[as.numeric(factor(Tick>3))],pH=pH[as.numeric(factor(Well))]) %>%
    dplyr::group_by(.,Well,pH,dye,fl) %>%dplyr::summarise(.,counts=mean(counts))
}

################################
grab24e<-function(u){
  pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
  readxl::read_excel(u,sheet="Level") %>%
    dplyr::mutate(.,fl=u) %>%
   dplyr::select(.,counts=contains("pH Corrected Em."),Tick,Well,fl)%>%
   dplyr::filter(.,Tick %in% tickfilter.B(Tick)) %>%
    dplyr::mutate(.,Tick=as.numeric(factor(Tick))) %>%
    dplyr::mutate(.,dye = c("CL","PR")[as.numeric(factor(Tick>3))],pH=pH[as.numeric(factor(Well))]) %>%
    dplyr::group_by(.,Well,pH,dye,fl) %>%
    dplyr::summarise(.,counts=mean(counts))
}
###############
grabXF24<-function(u){
  pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
  readxl::read_excel(u,sheet="Levels",startRow = 12,readxl = FALSE,skipEmptyRows=FALSE) %>%
    dplyr::mutate(.,fl=u) %>%
   dplyr::select(.,counts=contains("pH.Cor..Em." ),Tick,Well,fl) %>%
   dplyr::filter(.,Tick %in% tickfilter.B(Tick)) %>%
    dplyr::mutate(.,Tick=as.numeric(factor(Tick))) %>%
    dplyr::mutate(.,dye = c("CL","PR")[as.numeric(factor(Tick>3))]) %>%
    dplyr::mutate(.,pH=pH[as.numeric(factor(Well))]) %>%
    dplyr::group_by(.,Well,pH,dye,fl) %>%
    dplyr::summarise(.,counts=mean(counts))
}
##################
mungelist<-list(grab24e,grab96e,grabXF24,grabXFp)
####function to run pKa
pKaOLD<-function(pHFluor,MFBatch,Platform,Directory){
  list.files(path=Directory,pattern='xlsx',full.names = TRUE)   %>%
    lapply(.,mungelist[[as.numeric(Platform)]]) %>% 
    Reduce(x=.,f='rbind') %>%
    write.csv(x=.,file=file.path(Directory,"data.csv"),row.names=F)
  createRmd(pHFluor,MFBatch,Directory) %>%
    writeLines(text=.,con=file.path(Directory,paste0(pHFluor,"pKa.Rmd")),sep="\n")
  knit(input=file.path(Directory,paste0(pHFluor,"pKa.Rmd")),output=file.path(Directory,paste0(pHFluor,"pKa.md")))
  knit2html(input=file.path(Directory,paste0(pHFluor,"pKa.md")),
            output=file.path(Directory,paste0(pHFluor,"pKa.html")))
  file_in<-file.path(Directory,paste0(pHFluor,"pKa.html"))
  file_out<-gsub(".html",'.pdf',file_in)
  Pandoc_string<-paste0('pandoc -s ',file_in,' -o ',file_out)
  system(Pandoc_string)
}

pKa<-function(pHFluor,MFBatch,Platform,Directory){
  FileOut<-file.path(Directory,paste0(pHFluor,"pKa.Rmd"))
  list.files(path=Directory,pattern='xlsx',full.names = TRUE)   %>%
    lapply(.,mungelist[[as.numeric(Platform)]]) %>% 
    dplyr::rbind_all() %>%
    write.csv(x=.,file=file.path(Directory,"data.csv"),row.names=F)
  createRmd(pHFluor,MFBatch,Directory) %>%
    writeLines(text=.,con=file.path(Directory,paste0(pHFluor,"pKa.Rmd")),sep="\n")
  rmarkdown::render(input=FileOut)
}
createRmd<-function(pHFluor,MFBatch,Directory){
  pKaRmd<-readLines(system.file("rmd/pKaTemplate.Rmd", package="PipeFish"))
  pKaRmd[16]<-gsub('data.csv',file.path(Directory,"data.csv"),pKaRmd[16])
  pKaRmd[7]<-gsub('XBATCHX',MFBatch,pKaRmd[7])
  pKaRmd[2]<-gsub('XLOTX',pHFluor,pKaRmd[2])
  pKaRmd[45]<-gsub('pKA',paste0('pKa for ',pHFluor),pKaRmd[45])
  pKaRmd
}




