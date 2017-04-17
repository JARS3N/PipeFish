.sharpen<-function(x){
  require(magrittr)
  base64enc::base64decode(x) %>%
    rawToChar() %>%
    {
      if (grepl('%%', .)) {
        strsplit(., split = "%%") %>%
          unlist() %>%
          lapply(., strsplit, split = ";") %>%
          lapply(unlist) %>%
          {
            setNames(.[[1]], .[[2]])
          }
      } else{
        .
      }
    }
}
