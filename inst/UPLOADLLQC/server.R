library(shiny)
library(dplyr)
shinyServer(function(input, output) {

observeEvent(input$send,{
  print("ok")
  dir <- choose.dir()
  PipeFish::XLSXos(dir)
  if (!is.na(dir)) {
  list.files(path=dir,full.names = T,pattern = 'xlsx') %>%
  lapply(.,mungeLL) %>%
  lapply(.,UPLOADLL)
  message("complete")
  }else{
  message('no directory selected')
  }
}


)

})
