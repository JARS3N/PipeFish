.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    
CheckforDataStash<-function(){
  LISTlib<-unlist(lapply(.libPaths(),function(u){all(dir.exists(file.path(u,"Datastash")))}))
  if (!(TRUE %in% LISTlib)){
    dir.create(file.path((.libPaths()[1]),DS)) 
  }
}


CacheDBinfo<-function(A,B,C,D,E){
ConnectInfo<-c( 'dbname'= A,
                'user'= B,
                'password'= C,
                'host'= D,
                'port'= E)
LISTlib<-unlist(lapply(.libPaths(),function(u){all(dir.exists(file.path(u,"Datastash")))}))
saveRDS(ConnectInfo,
file.path((.libPaths()[LISTlib]),
"DataStash",
"DBinfo.RDS"))
}

