library(shiny)
library(dplyr)
shinyServer(function(input, output) {

observeEvent(input$send,{
  print("ok")
  dir <- choose.dir()
  if(input$CB==TRUE){PipeFish::XLSXos(dir)}
  if (!is.na(dir)) {
  list.files(path=dir,full.names = T,pattern = 'xlsx') %>%
  lapply(.,PipeFish::mungeLL) %>%
  lapply(.,PipeFish::UPLOADLL)
  message("complete")
  }else{
  message('no directory selected')
  }
}


)

})
