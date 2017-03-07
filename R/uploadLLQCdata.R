
UPLOADLL <- function(LLdat){
ConnectInfo <- DataStash::Triton()
require(RMySQL)
require(dplyr)

dmy_db <- dplyrCON()
n <- dmy_db %>%
  tbl('instqcllmeta') %>%
  select(.,file) %>%
  mutate(check = LLdat$meta$file == file) %>%
  filter(check == T) %>%
  summarise(n=n()) %>%
  collect() %>% unlist
# then if n we can upload,else skip

if(n == 0){
  ConnectInfo <- DataStash::Triton()
  my_db <- rmysqlCon()
  dbWriteTable(my_db, name = "instqcllmeta",value = LLdat$meta,
               append = T,overwrite = F,row.names=FALSE)


 runID <- dmy_db %>%
    tbl('instqcllmeta') %>%
    filter(file == LLdat$meta$file) %>%
    select(ID) %>%
    collect()

#uploadinstQClldata
 LLdat$data %>%
   mutate(.,MetaID = runID$ID) %>%
   dbWriteTable(my_db, name = "instqclldata",value = .,
                append = T,overwrite = F,row.names = FALSE)

 dbDisconnect(my_db)
}#end if bracket
rm(dmy_db)
gc()
}#final bracket



