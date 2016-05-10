.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    CheckforDataStash()
}
CacheDBinfo<-function(A,B,C,D,E){
  ConnectInfo<-c( 'dbname'= A, 'user'= B,'password'= C,'host'= D,'port'= E)
  savepath<-file.path(.libPaths()[LISTlib("Datastash")],"DataStash","DBinfo.RDS")
  saveRDS(ConnectInfo,savepath)
}
CheckforDataStash<-function(){
  LISTlib<-LISTlib("Datastash")
  if (!(TRUE %in% LISTlib)){
    dir.create(file.path((.libPaths()[1]),DS)) 
  }}
  
  DBinfo<-function(){
  fp<-file.path((.libPaths()[LISTlib("Datastash")]),"DataStash","DBinfo.RDS")
  readRDS(fp)
  }

LISTlib<-function(q){
unlist(lapply(.libPaths(),function(u){all(dir.exists(file.path(u,q)))}))
}
