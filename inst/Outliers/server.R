
library(shiny)
library(PipeFish)

shinyServer(function(input, output, session) {

 session$onSessionEnded(function() {
    stopApp()
  })

 
 observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })


    observe({
        if(input$BB > 0 ){
            DIR<-choose.dir()
            if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
            DF<-OLgrbs(DIR)
            svpth<-file.path(DIR,paste0(input$expnm,".csv"))
            output$session <- renderText(svpth)
            output$test1 <- renderTable({DF})
            write.csv(DF,file=svpth)
            require(Cairo)
            AVGplot<-PipeFish::plotOLAVGS(DF,input$expnm)
            CTGplot<-PipeFish::plotOLCTG(DF,input$expnm)
            ggsave(plot =AVGplot,file.path(DIR,paste0(input$expnm,"AVGplot.png")),
                  type = "cairo-png",dpi=600)
            ggsave(plot =CTGplot,file.path(DIR,paste0(input$expnm,"CTGplot.png")),
                   type = "cairo-png",dpi=600)
        }
    })


})
