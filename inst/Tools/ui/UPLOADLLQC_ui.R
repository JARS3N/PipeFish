mainPanel(
  h3("PipeFish::Upload Instrument QC Light Leak Data"),
  br(),
  p("Select directory containing xlsx files from light leak utility test"),
  checkboxInput("CB", "open and save XLSX files first", value = TRUE, width = NULL),
  actionButton('send', "Upload Data", icon = icon("cog", lib = "glyphicon"), width = NULL)
)