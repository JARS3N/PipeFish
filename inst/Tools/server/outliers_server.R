observeEvent(input$BB,{
    try(OL_DIR<-choose.dir(),silent=T)
    if(!is.na(OL_DIR)){
    if(input$CB==TRUE){
      PipeFish::Outandsave(OL_DIR);
      OL_DIR<-file.path(OL_DIR,'export')
      }
    DF<-OLgrbs(OL_DIR)
    svpth<-file.path(OL_DIR,paste0(input$expnm,".csv"))
    output$session <- renderText(svpth)
    output$test1 <- renderTable({DF})
    write.csv(DF,file=svpth)
    require(Cairo)
    AVGplot<-PipeFish::plotOLAVGS(DF,input$expnm)
    CTGplot<-PipeFish::plotOLCTG(DF,input$expnm)
    ggsave(plot =AVGplot,file.path(OL_DIR,paste0(input$expnm,"AVGplot.png")),
           type = "cairo-png",dpi=600)
    ggsave(plot =CTGplot,file.path(OL_DIR,paste0(input$expnm,"CTGplot.png")),
           type = "cairo-png",dpi=600)
    }else{
      message('no directory selected')
    }
})