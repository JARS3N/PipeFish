ReadBarcodeTimes<-function(u){
  list.files(path=u,pattern='.log') %>%
 split(.,.) %>%
  lapply(.,ReadBarcodeTime)%>%
    bind_rows(.)}

ReadBarcodeTime<-function(x){
  readLines(x) %>%
  data.frame(k=.) %>%
  filter(.,grepl(pattern="ReadCartridgeBarcode",k)) %>%
separate(.,col=k,sep=";",c("A","B","C","D","E")) %>%
(function(u){u$A}) %>%
  strptime(., "%Y-%m-%d %H:%M:%OS") %>%
  data.frame(Time=.,event=c("pre","post")) %>%
  (function(u){data.frame(pre=u$Time[u$event=="pre"],post=u$Time[u$event=="post"])}) %>%
  mutate(.,dif=as.difftime(post-pre,format="%OS"))}
