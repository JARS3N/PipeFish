library(shiny)
library(dplyr)
library(shinydashboard)

function(input, output,session) {
  observeEvent(input$menu,{
    print(input$menu)

    source(
       file.path("server",paste0(input$menu,"_server.R")),
       local = TRUE)$value

    output$dvi<-renderUI({

      source(
        file.path("ui",paste0(input$menu,"_ui.R")),
        local = TRUE)$value

    })
  })

}
