.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    #CheckforDataStash();
    autoUpGithub('PipeFish')
}
# REMOVED THE NEED FOR CHECK DATASTASH BY JUST CREATING A PACKAGE FOR DATASTASH,AVOIDS ALL THE OTHER MESS

#CacheDBinfo<-function(A,B,C,D,E){
#names(A)<-NULL;names(B)<-NULL;names(C)<-NULL;names(D)<-NULL;names(E)<-NULL;
#  ConnectInfo<-c( 'dbname'= A, 'user'= B,'password'= C,'host'= D,'port'= E)
#  savepath<-file.path(.libPaths()[LISTlib("DataStash")],"DataStash","DBinfo.RDS")
#  saveRDS(ConnectInfo,savepath)
#}
#CheckforDataStash<-function(){
#  LISTlib<-LISTlib("DataStash")
#  if (!(TRUE %in% LISTlib)){
#    dir.create(file.path((.libPaths()[1]),"DataStash")) 
#  }}
  
  DBinfo<-function(){
  fp<-file.path((.libPaths()[LISTlib("DataStash")]),"DataStash","DBinfo.RDS")
  readRDS(fp)
  }

LISTlib<-function(q){
unlist(lapply(.libPaths(),function(u){all(dir.exists(file.path(u,q)))}))
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
    devtools::install_github(gsub("https://github.com/","",pkg),quite=TRUE)
  }else{message(paste0("Github version is identical to installed \n no update for ",pack))}
}


