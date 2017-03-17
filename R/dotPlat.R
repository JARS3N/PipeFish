#determine platform by Instrument string...or if it is being read as number instead of string
.Plat<-function(Instrument){
  s1<-as.character(substr(Instrument,1,1))
  s2<-as.character(substr(Instrument,2,2))
  if(s1=="2"){
    return("XF96")
  }else{
    return(c("XFe96","XFe24","XFp")[as.numeric(s2)])
  }
}
