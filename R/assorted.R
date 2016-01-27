

checkforpackage<-function(A){y<-installed.packages()[,1];chk<-(A %in% y);if (chk==FALSE){install.packages(A,repos='http://cran.us.r-project.org',dependencies=TRUE) }}






XLSXos<-function(u=getwd()){
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
