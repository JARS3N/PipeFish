#### General function to munge data for Glyco Buffer Capacity
G2BC<-function(fl){
require(dplyr)
require(readxl)
require(tidyr)
# determine assay used
 assay<- gregexpr("inj|titr", basename(fl)) %>%  regmatches(basename(fl), .) %>% unlist()
# read in excel sheet
 fl%>% readxl::read_excel(.,sheet="Raw")%>% 
    select(.,Measurement,Tick,Well,pH) %>% 
# grab last 3 ticks of measure and take the mean
  group_by(.,Measurement) %>% 
    filter(.,Tick %in% c(max(Tick)-c(2,1,0))) %>% 
  ungroup(.) %>% 
    group_by(.,Measurement,Well) %>% 
    summarize(.,meanpH=mean(pH)) %>% 
    ungroup(.) %>% 
   ## Bridge,deteremines munge difference for either injection assay or titration assay
   ({
     if (assay=='inj'){
       . %>% 
         mutate(pHM=c("m0uL","m20uL","m42uL","m67uL")[Measurement]) %>% 
         select(.,-Measurement) %>% 
         tidyr::spread(.,key=pHM,value=meanpH) %>% 
         rename(.,startpH=m0uL) %>% 
         tidyr::gather(.,volHCl,finalpH,-Well,-startpH) %>% 
         mutate(volHCl=gsub("[A-Z,a-z]","",volHCl)  %>% as.numeric())
     }else{
       . %>%
       mutate(.,pHM=c("startpH","finalpH")[Measurement]) %>% 
         select(.,-Measurement) %>% 
         tidyr::spread(.,key=pHM,value=meanpH) %>% 
         mutate(Col=as.numeric(gsub("[A-Z]","",Well))) %>% 
         mutate(.,volHCl=rep(c(0,20,42,67),3)[Col]) %>% 
         select(.,-Col)
     }
   }) %>% 
   mutate(.,deltapH=startpH-finalpH) %>% 
   mutate(.,HClMolarity=5/1000) %>% 
   mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>% 
   mutate(.,volMedia=180/(1000^2)) %>% 
   mutate(.,bufferCapacity=molesH/(deltapH*volMedia)) %>% 
   mutate(file=fl)
}
  
  
###### app for G2BC

G2BCAPP<-function(){
require(shiny)
require(PipeFish)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("PipeFish::Glyco2BufferCapacity"),
  br(),
  mainPanel(
    textInput('expnm',"Name of Experiment", value = "OutlierAnalysis", width = NULL),
    checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
    br(),
    actionButton("BB","Run Analysis"),
    br(),
    actionButton('Quit','Quit',icon=icon('remove-sign',lib='glyphicon')),
    br(),br(),
    textOutput("session"),
    tableOutput("test1")
  )))
  

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  
  
  observe({
    if(input$BB > 0 ){
      DIR<-choose.dir()
      if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
      DF<-list.files(DIR,full.names=T) %>% lapply(.,G2BC) %>% bind_rows()
      svpth<-file.path(DIR,paste0(input$expnm,".csv"))
      output$session <- renderText(svpth)
      output$test1 <- renderTable({DF})
      write.csv(DF,file=svpth,row.names = F)
    }
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

}
  
  
  
  
