
library(shiny)
library(PipeFish)
scriptlist<-PipeFish::available_scripts()
shinyUI(
  fluidPage(title="PipeFish::Scripts",
  fluidRow(
    column(h4("Select Sript"),width=4,
selectInput(inputId="Scripts",label="",choices=scriptlist,multiple=FALSE,selected=NULL)
          ),#col select
    column(width=5,
      actionButton(inputId="Launch",label="Launch Script", icon = icon('rocket'))
    )

),#fluidrow
fluidRow(column(h4("Description"),width=9,p("Descriptions etc etc etc ...")))
)#fluidpage
)#ui
