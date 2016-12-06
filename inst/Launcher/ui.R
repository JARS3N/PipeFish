
library(shiny)
library(PipeFish)
#scriptlist<-PipeFish::available_scripts()
shinyUI(
  fluidPage(title="PipeFish::Scripts",
            fluidRow(
              column(h4("Select Script"),width=4,
                     selectInput(inputId="Scripts",label="",
                          choices=c(
                            "WavetoExcel"=1,
                            "XFDtoWave"=2,
                            "Outliers"=3,
                            "pKa(fromAsyr)"=4,
                            "UploadDryQC"=5,
                            "reprintBarcodes"=6
                          )       
                                   
                                   
                                   ,multiple=FALSE,selected=NULL)
              ),#col select
              column(width=5,
                     actionButton(inputId="Launch",label="Launch Script", icon = icon('rocket'))
              )
              
            ),#fluidrow
            fluidRow(column(h4("Description"),width=9,textOutput("MSG")))
  )#fluidpage
)#ui
