  observeEvent(input$goButton,{
    library(RMySQL)
    library(XML)
    output$MSG <- renderText("Ready")

      output$MSG <- renderText("Select Directory")
      DIR<-choose.dir();
      if(!is.na(DIR)){
      output$MSG <- renderText("Munging Data...")

      DF<-DIR %>%
        list.files(path=.,pattern='asyr',full.names=TRUE) %>%
        lapply(.,XML::xmlTreeParse) %>%
        lapply(.,PipeFish::Collect) %>%
        lapply(.,PipeFish::whichAssay) %>%
        dplyr::bind_rows(.) %>%
        select(.,-O2.IntialReferenceDelta,-pH.IntialReferenceDelta) %>%
        as.data.frame(.)


      output$DF<-shiny::renderDataTable(DF)
      output$MSG <- renderText("Communicating with Database")
      output$MSG <- renderText("Writing to Database")
      PipeFish::UploadsCC(DF)
      output$MSG <- renderText("Complete")
      }else{
        message("no directory selected")
      }
    })