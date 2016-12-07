library(shiny)
shinyServer(function(input, output) {
  # "WavetoExcel"=1,
  # "XFDtoWave"=2,
  # "Outliers"=3,
  # "pKa(fromAsyr)"=4,
  # "UploadDryQC"=5,
  # "reprintBarcodes"=6 
  # "UploadCCXFp.R =7
  
  Rfiles<-c(
    "Outandsave.R",
    "scTorq.R",
    "PipeOL.R",
    "shinyasyr_pKa.R",
    "getDQUP.R",
    "reprintBC.R" ,
    "UploadCCXFp.R"
  )
  
  descriptions=c(
  "Convert a wave file to excel(xlsx) and save",
  "Convert XFD reader file to Wave (asyr)",
  "Select folder or files for outlier analysis",
  "Select folder of asyr files for pKa analysis",
  "Select a DryQC excel file to upload to database",
  "Resave a previously created lot of catridge barcodes to the location targeted by Bartender software",
   "Upload Cartridge Wet QC to database using Wave (asyr) files"
  )
  observeEvent(input$Scripts,{
    output$MSG <- renderText(descriptions[as.numeric(input$Scripts)])
    
  })
  
  observe({
    if(input$Launch > 0){
      filz=Rfiles[as.numeric(input$Scripts)]
      message(paste0("selected: ",Rfiles[as.numeric(input$Scripts)]))
      message("Loading Requested Shiny App...")
      A<-"Rscript.exe" 
      B<-" -e "  
      C<-shQuote(file.path(system.file(package = "PipeFish","scripts"),filz))
      D<-shQuote(paste0("source(",C,",local=TRUE)"))
      system(paste0(A,B,D))
      
    }
  })
})
