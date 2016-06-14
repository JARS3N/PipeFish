library(shiny)
library(PipeFish)
library(dplyr)
library(rio)
library(rmarkdown)
shinyServer(function(input, output, session) {

observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })

observe({
  if(input$GO > 0 ){
  x<- normalizePath(choose.files(multi=FALSE))
      if (grepl("[.]xlsx",basename(x)==TRUE){
      setwd(dirname(x))
      out<-gsub(".xlsx",".rmd",basename(x))
      readLines(file.path(system.file(package='PipeFish',path='rmd'),"QualifyingMediaRun.rmd")) %>%
      gsub("FILE",basename(x),.) %>%
      writeLines(text=.,con=out,sep="\n")
      rmarkdown::render(input=out)
      }
  }
})


})




