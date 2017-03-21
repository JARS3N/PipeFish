.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    #CheckforDataStash();
    autoUpGithub('PipeFish')
    checkUsr()
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


checkUsr<-function(){
  require(dplyr)
  getLink<-function(){
    getsubd<-function(u){
      marker<-"Connection-specific DNS Suffix  . :"
      shell("ipconfig",intern=T) %>%
        grep(marker,.,value=T) %>%
        gsub(marker,"",.) %>%
        gsub(" ","",.) %>%
        unique() %>%
        .[.!=""]
    }
    
    data.frame(user=Sys.getenv("USERNAME"),
               comp=Sys.getenv("COMPUTERNAME"),
               domain=getsubd()) %>% 
      mutate(
        link=paste0("http://",comp,".",domain,":9999"))
  }#getLink
  
  addUsr<-function(info){
    require(RMySQL)
    DB<-PipeFish::rmysqlCon()
    dbWriteTable(DB, name="pfusr",value=info,
                 append=T,overwrite = F,row.names=FALSE)
    dbDisconnect(DB)
    saveRDS(Sys.Date(),file.path(system.file(package="door"),"completed.RDS"))
  }#addUsr
  
  getUsrs<-function(){
    db<-PipeFish::dplyrCON()
    db %>% tbl('pfusr') %>% 
      select(user) %>% 
      distinct() %>% 
      collect() %>% 
      .$user %>%
      unname()
  }#getUsrs
  
  if(!file.exists(system.file(package="door","completed.RDS"))){
    info<-getLink()
    if(exists(info)){
      all<-getUsrs()
      if(!any(info$user %in% all)){addUsr(info)}
    }
  }
}
}
