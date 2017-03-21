# checkUsr<-function(){
#   require(dplyr)
#   getLink<-function(){
#     getsubd<-function(u){
#       marker<-"Connection-specific DNS Suffix  . :"
#       shell("ipconfig",intern=T) %>%
#         grep(marker,.,value=T) %>%
#         gsub(marker,"",.) %>%
#         gsub(" ","",.) %>%
#         unique() %>%
#         .[.!=""]
#     }
#     
#     data.frame(user=Sys.getenv("USERNAME"),
#                comp=Sys.getenv("COMPUTERNAME"),
#                domain=getsubd()) %>% 
#       mutate(
#         link=paste0("http://",comp,".",domain,":9999"))
#   }#getLink
#   
#   addUsr<-function(info){
#     require(RMySQL)
#     DB<-PipeFish::rmysqlCon()
#     dbWriteTable(DB, name="pfusr",value=info,
#                  append=T,overwrite = F,row.names=FALSE)
#     dbDisconnect(DB)
#     saveRDS(Sys.Date(),file.path(system.file(package="door"),"completed.RDS"))
#   }#addUsr
#   
#   getUsrs<-function(){
#     db<-PipeFish::dplyrCON()
#     db %>% tbl('pfusr') %>% 
#       select(user) %>% 
#       distinct() %>% 
#       collect() %>% 
#       .$user %>%
#       unname()
#   }#getUsrs
#   
#   if(!file.exists(system.file(package="door","completed.RDS"))){
#     info<-getLink()
#     if(exists('info')){
#       all<-getUsrs()
#       if(!any(info$user %in% all)){addUsr(info)}
#     }
#   }
# }
