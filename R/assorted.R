

checkforpackage<-function(A){y<-installed.packages()[,1];chk<-(A %in% y);if (chk==FALSE){install.packages(A,repos='http://cran.us.r-project.org',dependencies=TRUE) }}
gradeOL<-function(u){c("A","B","C","D")[c(u < 5,u >=5 & u <10,u >=10 & u <20,u >= 20)]}
TC<-function(x){
  cumdif.counts<-diff(x$counts)
  injection<-x$Time[which(cumdif.counts==min(cumdif.counts))-1]
  period<-injection + 30
  start<-x$counts[x$Time==x$Time[which(abs(x$Time-injection)==min(abs(x$Time-injection)))]]
  endP<-x$counts[x$Time==x$Time[which(abs(x$Time-period)==min(abs(x$Time-period)))]]
  endT<-min(sapply(which(abs(x$Time-period)==min(abs(x$Time-period))):(length(x$Time)-4),function(u){mean(x$counts[u:u+4])})) #avg of 5 points
  deltaP<-start-endP
  deltaT<-start-endT
  abs(period/(log(1/(1-(deltaP/deltaT)))))
}

pO2<-function(TC,atm=760){
  HC<-function(TC){(-0.0000058333*TC^3+0.0001821*TC^2+0.072405*TC+2.5443)*10000}
  vp<-function(TC){0.0456*TC^2-0.8559*TC+16.509}
  DO<-function(TC,vp,ap=atm){
    if(TC>=0 & TC<30){coef<-.678;adj<-35}else{if(TC>=30 & TC<=50){coef<-.827;adj<-49}}
    ((ap-vp)*coef)/(adj+TC)
  }
  DO(TC,vp(TC),atm)*(1/1000)*(1/32)*(18/1000)*HC(TC)*atm
}


OLgrb<-function(u){
  import(file=u,sheet='Level') %>%
    select(Well,pH,O2=contains("O2 (mmHg)" )) %>%
    mutate(pHdif=abs(pH-7.4),O2dif=abs(O2-152))  %>%
    group_by(Well) %>%
    summarise(mxO2=max(O2dif)) %>%
    mutate(grade=sapply(mxO2,gradeOL)) %>%
    mutate(Lot=import(u,sheet='Assay Configuration')[27,2]) %>%
    mutate(sn=import(u,sheet='Assay Configuration')[26,2]) %>%
    mutate(Instrument=import(u,sheet='Assay Configuration')[35,2]) %>%
    mutate(fl=u)
}

XLSXos<-function(u){
system(paste0(
"Cscript",
" ",
shQuote(normalizePath(system.file("vbs/XLOSC.vbs", package="PipeFish"))),
" ",
shQuote(normalizePath(u))
))
}

PipeFishGUI<-function(){shell.exec(system.file(path='/gui/PipeFish.exe',package='PipeFish'))}

LNK<-function(){
  system(paste0(
    "Cscript",
    " ",
    shQuote(normalizePath(system.file("vbs/PFlink.vbs", package="PipeFish"))),
    " ",
    shQuote(normalizePath(system.file("gui/PipeFish.exe",package="PipeFish")))
  ))
}
