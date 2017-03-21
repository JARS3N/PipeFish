.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    #CheckforDataStash();
    autoUpGithub('PipeFish')
   # if(Sys.getenv("USERNAME")=='mapedone'){
     #   for(i in 1:30) print("HELLO MATT!!!")
     #   }
}

givename<-function(u,splits=": "){
  Q <-strsplit(u,split=splits);
  out<-Q[[1]][2];
  names(out)<-Q[[1]][1];
  out
}

autoUpGithub<-function(pack){
  testUrl <- function(url) {
    out <- tryCatch(
      {
        readLines(con=url, warn=FALSE,n=1)
      },error=function(cond) {
        return(NA)
      },warning=function(cond) {
        return(FALSE)
      },finally={})
    return(out)
  }
  pkg<-packageDescription(pack)$URL
  GETS<-paste0("https://raw.githubusercontent.com/",pkg,"/master/DESCRIPTION")
  if (testUrl(GETS)==FALSE){return(message("Can't seem to contact github repo\n will not update."))}
  ongit<- gsub("Version: ","",grep("Version: ",readLines(GETS,warn=F),value=T))
  if (ongit!=utils::packageVersion(pack)){
    message(paste0("Github version differs from installed version \n update",pack))
    devtools::install_github(gsub("https://github.com/","",pkg),quite=TRUE,dependencies = T,quick=T)
  }else{message(paste0("Github version is identical to installed \n no update for ",pack))}
}



