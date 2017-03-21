observeEvent(input$BB,{
  DIR<-choose.dir()
  if(!is.na(DIR)){
  if(input$CB==TRUE){PipeFish::torquemada(DIR)}
  PipeFish::asyr_pKa(input$pHFluor,input$MFBatch,input$Platform,DIR)
  }
})