#### General function to munge data for Glyco Buffer Capacity
G2BC<-function(fl){
require(dplyr)
require(readxl)
require(tidyr)
# determine assay used
 assay<- gregexpr("inj|titr", tolower(basename(fl))) %>%  regmatches(basename(fl), .) %>% unlist() %>% tolower()
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
###### function to use for QC


G2BC2<-function(fl){
  require(dplyr)
  require(readxl)
  require(tidyr)
  AC<-function(fl){ 
    h<-readxl::read_excel(fl,sheet="Assay Configuration")[,c(1,2)] 
    AClist<-unlist(h[,2]) %>% set_names(unlist(h[,1])) 
    data.frame(Lot=paste0(AClist["Cartridge Type"] %>% unname(),
                              AClist["Cartridge Lot"] %>% unname()),
               sn=AClist["Cartridge Serial"]%>% unname(),
               fl=AClist["Assay Name"]%>% unname())
  }
  G2F1<-{
    . %>% 
      readxl::read_excel(.,sheet="Raw")%>% 
      select(.,Measurement,Tick,Well,pH) %>% 
      # grab last 3 ticks of measure and take the mean
      group_by(.,Measurement) %>% 
      filter(.,Tick %in% c(max(Tick)-c(2,1,0))) %>% 
      ungroup(.) %>% 
      group_by(.,Measurement,Well) %>% 
      summarize(.,meanpH=mean(pH)) %>% 
      ungroup(.) %>% 
      mutate(.,pHM=c("startpH","finalpH")[Measurement]) %>% 
      select(.,-Measurement) %>% 
      tidyr::spread(.,key=pHM,value=meanpH)
  }
  FUNs<-list(
    '96'=function(u){
      u %>%
        mutate(.,Col=gsub("[A-Z]","",Well) %>% as.numeric()) %>% 
        mutate(.,volHCl=rep(c(0,20,42,67),3)[Col]) %>% 
        select(.,-Col) %>% 
        mutate(.,deltapH=startpH-finalpH) %>% 
        mutate(.,HClMolarity=5/1000) %>% 
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>% 
        mutate(.,volMedia=180/(1000^2)) %>% 
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia)) 
    },
    '24'=function(u){
      u %>% 
        mutate(.,volHCl=c(rep(62,6),rep(2*62,6),rep(3*62,6),rep(0,6))) %>% 
        mutate(.,deltapH=startpH-finalpH) %>% 
        mutate(.,HClMolarity=5/1000) %>% 
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>% 
        mutate(.,volMedia=500/(1000^2)) %>% 
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia)) 
    },
    '8'=function(u){
      u %>%  mutate(.,volHCl=c(0,20,42,67) %>% rep(.,2)) %>% 
        mutate(.,deltapH=startpH-finalpH) %>% 
        mutate(.,HClMolarity=5/1000) %>% 
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>% 
        mutate(.,volMedia=180/(1000^2)) %>% 
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia)) 
    }
  )
  
  G2F1(fl) %>% 
  {FUNs[[.$Well %>% unique() %>% length() %>% as.character()]](.)} %>% 
    merge(AC(fl))
  
}
###### app for G2BC

G2BCAPP<-function(){
require(shiny)
require(PipeFish)
require(dplyr)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("PipeFish::Glyco2BufferCapacity"),
  br(),
  mainPanel(
    textInput('expnm',"Name of Experiment", value = "BufferCapacityAnalysis", width = NULL),
    checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
    br(),
    actionButton("BB","Run Analysis"),
    br(),
    actionButton('Quit','Quit',icon=icon('remove-sign',lib='glyphicon')),
    br(),br(),
    textOutput("session"),
    tableOutput("test1")
  )))
  
server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$Quit, {
    stopApp(returnValue = invisible())
  })
  
  
  observe({
    if(input$BB > 0 ){
      DIR<-choose.dir()
      if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
      DF<-list.files(DIR,full.names=T,pattern='xlsx') %>% lapply(.,G2BC) %>% bind_rows()
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
  
  
  
  
