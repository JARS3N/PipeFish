TC<-function(x){
    cumdif.counts<-diff(x$counts)
    injection<-x$Time[which(cumdif.counts==min(cumdif.counts))-3]
    period<-injection + 30
    start<-x$counts[x$Time==x$Time[which(abs(x$Time-injection)==min(abs(x$Time-injection)))]]
    endP<-x$counts[x$Time==x$Time[which(abs(x$Time-period)==min(abs(x$Time-period)))]]
    endT<-min(sapply(which(abs(x$Time-period)==min(abs(x$Time-period))):(length(x$Time)-4),function(u){mean(x$counts[u:u+4])})) #avg of 5 points
    deltaP<-start-endP
    deltaT<-start-endT
    abs(period/(log(1/(1-(deltaP/deltaT)))))
}

PreProcessTC <-function(u){#where u is the csv file csv file
  require(dplyr)
  rio::import(u) %>%
  dplyr::select(.,-contains('LOW'),Time=contains('Time'),contains('High')) %>% 
  tidyr::gather('Chan','counts',-contains("Time"))%>%
  dplyr::mutate(Chan=gsub("High ","",Chan))  %>%
  dplyr::mutate(fl=u)
  }
  
  TCall<-function(u){#where u is the directory of csv files to run
  list.files(path=u,pattern="csv",full.names=TRUE) %>%
  lapply(.,PipeFish::PreProcessTC) %>%
  lapply(.,function(u){
          group_by(u,Chan,fl) %>%
          summarise(TC=PipeFish::TC(data.frame(Time=Time,counts=counts)))}
        )
  }
