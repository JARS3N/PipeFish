library(dplyr)
library(shinydashboard)
library(shiny)
sidebar <- dashboardSidebar(
  sidebarMenu(id='menu',
  menuItem("pKa", tabName = "pKa"),
  menuItem("ExportAsyrToExcel",
           tabName = "ExportAsyrToExcel"),
  menuItem("outliers",
           tabName = "outliers"),
  menuItem("uploadCC",
           tabName = "uploadCC"),
  menuItem("UPLOADLLQC",
           tabName = "UPLOADLLQC"),
  menuItem("InstrumentQCOLupload",
           tabName = "InstrumentQCOLupload")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'now',class = "active", uiOutput('dvi'))#,
  )
)

dashboardPage(
  dashboardHeader(title = "PipeFish"),
  sidebar,
  body
)
