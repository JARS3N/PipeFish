library(shiny)
shinyServer(function(input, output) {
  
  observe({
    if(input$Launch > 0){
      A<-"Rscript.exe" 
      B<-" -e "  
      C<-shQuote(file.path(system.file(package = "PipeFish","scripts"),input$Scripts))
      D<-shQuote(paste0("source(",C,",local=TRUE)"))
      system(paste0(A,B,D))
      
    }
    })
})
