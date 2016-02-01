
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    p("Export Folder of asyr files to Excel"),mainPanel(
    actionButton("EXP", "Export"),
    textOutput("session"))
))