.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    
CheckforDataStash<-function(){
 Lib<- (.libPaths())
  DS<-"DataStash"
Exists<-all(dir.exists(file.path(Lib,DS)))
  if(Exists==FALSE){
    dir.create(file.path((.libPaths()[1]),DS))}
}
}


CacheDBinfo<-function(A,B,C,D,E){
ConnectInfo<-c( 'dbname'= A,
                'user'= B,
                'password'= C,
                'host'= D,
                'port'= E)

saveRDS(ConnectInfo,
file.path((.libPaths()[1]),
"DataStash",
"DBinfo.RDS"))
}
