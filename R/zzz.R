.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    CheckforDataStash<-function(){
        if(dir.exists(file.path((.libPaths()),"Datastash"))==FALSE){
            dir.create(file.path((.libPaths()),"Datastash"))}
    }
}


CacheDBinfo<-function(A,B,C,D,E){
ConnectInfo<-c( 'dbname'= A,
                'user'= B,
                'password'= C,
                'host'= D,
                'port'= E)

saveRDS(ConnectInfo,
file.path((.libPaths()),
"DataStash",
"DBinfo.RDS"))
}
