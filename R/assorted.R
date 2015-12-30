

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
        select(data=.,Well,pH,O2=contains("O2 (mmHg)" )) %>%
        mutate(data=.,pHdif=abs(pH-7.4),O2dif=abs(O2-152))  %>%
        group_by(data=.,Well) %>%
        summarise(data=.,mxO2=max(O2dif)) %>%
        mutate(data=.,grade=sapply(mxO2,gradeOL)) %>%
        mutate(data=.,Lot=import(u,sheet='Assay Configuration')[27,2]) %>%
        mutate(data=.,sn=import(u,sheet='Assay Configuration')[26,2]) %>%
        mutate(data=.,Instrument=import(u,sheet='Assay Configuration')[35,2]) %>%
        mutate(data=.,fl=u)
}

XLSXos<-function(u){
system(paste0(
"Cscript",
" ",
shQuote(normalizePath(system.file("/vbs/XLOSC.vbs", package="PipeFish"))),
" ",
shQuote(normalizePath(u))
))
}

PipeFishGUI<-function(){shell.exec(system.file(path='/gui/PipeFish.exe',package='PipeFish'))}

LNK<-function(){
  system(paste0(
    "Cscript",
    " ",
    shQuote(normalizePath(system.file("/vbs/PFlink.vbs", package="PipeFish"))),
    " ",
    shQuote(normalizePath(system.file("/gui/PipeFish.exe",package="PipeFish")))
  ))
}

BuildPipeFish<-function(x){
    library(dplyr)
    x<-tempdir()
    warning("Creating Temp directory to download PipeFish Build files")
    setwd(x)
    warning("Downloading nodedbob master.zip")
download.file("https://github.com/geo8bit/nodebob/archive/master.zip",
              paste0(x,"/","master.zip"))
unzip(file.path(x,"master.zip"),exdir=x)
warning("Downloading PipeFistEtc files")
download.file("https://github.com/JARS3N/PipeFishEtc/archive/master.zip",
              file.path(x,"PipeFishEtc.zip"))
unzip(file.path(x,"PipeFishEtc.zip"),exdir=x)

list.files(file.path(x,"PipeFishEtc-master"),full.names=T) %>%
    lapply(X=.,function(u){file.copy(from=u,to=file.path(x,"nodebob-master",'app'),overwrite=TRUE)})
setwd(file.path(x,'nodebob-master'))
 if(.Platform$OS.type=="windows"){
     shell.exec(file.path(x,'nodebob-master','build.bat'))
     file.rename(from=file.path(x,"nodebob-master","release","nw.exe"),
                 to=file.path(x,"nodebob-master","release","PipeFish.exe"))
 }
if(.Platform$OS.type=="unix"){
    system(file.path(x,'nodebob-master','build.linux.sh'))
    file.rename(from=file.path(x,"nodebob-master","release","nw"),
                to=file.path(x,"nodebob-master","release","PipeFish"))
}
frm<-list.files(file.path(x,"nodebob-master","release"),full.names=T)
to<-file.path(system.file(package='PipeFish'),'gui')
file.copy(from=frm,to=to,overwrite=TRUE)
message("unlinking Temp directory")
unlink(x)
print("PipeFish Build and replace complete")
#need to add in the option to seect another final folder for updating the package build
}
