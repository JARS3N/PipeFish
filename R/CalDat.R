CalDat<-function(fl=file.choose()){
  GetValues<-function(J){
    lapply(seq_along(J),function(x){
      J[[x]] %>% XML::xmlSApply(.,XML::xmlValue) %>%
        as.numeric()}) %>% unlist()
  }
  A<-XML::xmlTreeParse(fl)
  O2<-A$doc$children$CalibrationData[["O2Data"]][["LedValues"]]
  PH<-A$doc$children$CalibrationData[["PHData"]][["LedValues"]]
  Well=paste0(sapply(LETTERS[1:8],rep,12),
              sprintf("%02d",c(1:12)))
  file<-A$doc$file
  Lot<-XML::xmlValue(A$doc$children$CalibrationData[["CartridgeLot"]]) %>%
    paste0("W",.)
  sn<-XML::xmlValue(A$doc$children$CalibrationData[["CartridgeSerial"]])

  data.frame(
    Well=Well,pH.LED=GetValues(PH),O2.LED=GetValues(O2),
    Lot=Lot,sn=sn,file=file

  )
}
