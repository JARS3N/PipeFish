  observeEvent(input$EXP,
                               {fldr<-choose.dir()
                               if(!is.na(fldr)){
                               PipeFish::Outandsave(fldr)
                               output$session <- renderText("Script Complete")
                               }
                               })
