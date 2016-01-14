
library(shiny)
library(PipeFish)

shinyServer(function(input, output, session) {

    #observe({
    #    if(input$getdir > 0){ DIR<-choose.dir();output$session <- renderText(DIR)}
    #})

    observe({
        if(input$BB > 0 ){
            DIR<-choose.dir()
            input$expnm
            DF<-getOLdata(DIR)
            svpth<-file.path(DIR,paste0(input$expnm,".csv"))
            output$session <- renderText(svpth)
            output$test1 <- renderTable({DF})
            write.csv(DF,file=svpth)
            require(Cairo)
            AVGplot<-plotOLAVGS(DF,input$expnm)
            CTGplot<-plotOLCTG(DF,input$expnm)
            ggsave(plot =AVGplot,file.path(DIR,paste0(input$expnm,"AVGplot.png")),
                  type = "cairo-png",dpi=600)
            ggsave(plot =CTGplot,file.path(DIR,paste0(input$expnm,"CTGplot.png")),
                   type = "cairo-png",dpi=600)
        }
    })


})
