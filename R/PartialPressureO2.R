
pO2<-function(TC,atm=760){
    HC<-function(TC){(-0.0000058333*TC^3+0.0001821*TC^2+0.072405*TC+2.5443)*10000}
    vp<-function(TC){0.0456*TC^2-0.8559*TC+16.509}
    DO<-function(TC,vp,ap=atm){
        if(TC>=0 & TC<30){coef<-.678;adj<-35}else{if(TC>=30 & TC<=50){coef<-.827;adj<-49}}
        ((ap-vp)*coef)/(adj+TC)
    }
    DO(TC,vp(TC),atm)*(1/1000)*(1/32)*(18/1000)*HC(TC)*atm
}


PartialPressureO2<-function(){
  library(shiny)
  library(ggplot2)
  library(ggthemes)
  pO2<-function(TC,atm=760){
    HC<-function(TC){(-0.0000058333*TC^3+0.0001821*TC^2+0.072405*TC+2.5443)*10000}
    vp<-function(TC){0.0456*TC^2-0.8559*TC+16.509}
    DO<-function(TC,vp,ap=atm){
      if(TC>=0 & TC<30){coef<-.678;adj<-35}else{if(TC>=30 & TC<=50){coef<-.827;adj<-49}}
      ((ap-vp)*coef)/(adj+TC)
    }
    DO(TC,vp(TC),atm)*(1/1000)*(1/32)*(18/1000)*HC(TC)*atm
  } 
  
  shinyApp(
    
   ui= shinyUI(fluidPage(
      titlePanel("Partial Pressure O2"),
      sidebarLayout(
        sidebarPanel(width=2,
                     numericInput('TMP','Target Temp(C)',value=37,min=0,max=40),
                     numericInput('ATM','atm(mmHg)',value=760)
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    )),
   server=shinyServer(function(input, output) {
     
     output$distPlot <- renderPlot({
       
       selectedData <- reactive({
         data.frame(
           temp=0:40,
           PP=sapply(0:40,pO2,atm=input$ATM)       
         )
       })
       ggplot(selectedData(),aes(temp,PP))+
         geom_line(col='red')+
         geom_point(aes(x=input$TMP,y=pO2(input$TMP,input$ATM)),size=3,alpha=.5,col='green')+
         theme_bw()+
         ylab('mmHg')+
         xlab('Temp(C)')+
         geom_text(aes(x=input$TMP,y=pO2(input$TMP,input$ATM),label=round(pO2(input$TMP,input$ATM),3)),vjust = -0.6,
                   family = "Times New Roman",size=10)+
         ylim(c(min(selectedData()$PP),max(selectedData()$PP)+1))
     })
     
   })
    
    
    
  )
  
  
  
}
