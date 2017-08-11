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
 require(dplyr)
  list.files(path=u,pattern="csv",full.names=TRUE) %>%
    lapply(.,PipeFish::PreProcessTC) %>%
    bind_rows(.) %>% 
    group_by(.,Chan,fl) %>% 
    summarize(TC=PipeFish::TC(data.frame(Time=Time,counts=counts)))
}
###### will replace TC with TC2 when it appears completed.
TC2<-function(x){
    cumdif.counts<-diff(x$counts)
    injection<-x$Time[which(cumdif.counts==min(cumdif.counts))-3]
    period<-injection + 30
    atmi<-abs(x$Time-injection)#abs_time_minus_injection
    atmp<-abs(x$Time-period)#abs_time_minus_period
    start<-x$counts[x$Time==x$Time[which(atmi==min(atmi)]]
    endP<-x$counts[x$Time==x$Time[which(atmp)==min(atmp)]]
    truncated_counts<-x$counts[which(atmp==min(atmp)):(length(x$Time)-4)]
    delta_time<-start-lowest_avg_n_counts(truncated_counts)
    delta_period<-start-endP
    abs(period/(log(1/(1-(delta_period/delta_time)))))
}

lowest_avg_n_counts<-function(U,n=5){
    i<-seq_along(U)
    A<-na.omit(lag(i,n-1))
    B<-na.omit(lead(i,n-1))
    mapd<-mapply(function(A,B){mean(U[A:B])},A,B)
    min(mapd)
}
