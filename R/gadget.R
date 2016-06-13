MassAssayExporteR <- function() {
  require(shiny)
  require(PipeFish)
  require(miniUI)
  
ui<-miniPage(fluidPage(
  p("Export Folder of asyr files to Excel"),mainPanel(
    actionButton("EXP", "Export"),
    textOutput("session"),
    actionButton("DONE", "DONE"))
))

server<-function(input,output,session) {
  
  stuffHappens<-  observeEvent(input$EXP,
                               {fldr<-choose.dir()
                               PipeFish::Outandsave(fldr)
                               output$session <- renderText("Script Complete")
                               })
  observeEvent(input$DONE, {
    stopApp(returnValue = invisible())
  })
}
  runGadget(ui, server,viewer = dialogViewer("MassAssayExporteR", width = 400 ,height = 400))
}
