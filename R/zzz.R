.onLoad <- function(libname = find.package("PipeFish"), pkgname = "PipeFish") {
    CheckforDataStash<-function(){
        if(dir.exists(file.path((.libPaths()),"Datastash"))==FALSE){
            dir.create(file.path((.libPaths()),"Datastash"))}
    }
}
