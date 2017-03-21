#ExportAsyrToExcel_ui.R
mainPanel(
  p("Export Folder of asyr files to Excel"),
  actionButton("EXP", "Export"),
  br(),br(),br(),
  actionButton("Quit", "Quit"),
  textOutput("session")
  )