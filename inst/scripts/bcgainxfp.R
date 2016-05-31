checkforpackage<-function(A){y<-installed.packages()[,1];chk<-(A %in% y);if (chk==FALSE){install.packages(A,repos='http://cran.us.r-project.org',dependencies=TRUE) }}
checkforpackage('dplyr')
checkforpackage('rio')
checkforpackage('tidyr')
checkforpackage('methods')
library(methods)
library(dplyr)
library(rio)
library(tidyr)
loc<-choose.dir();setwd(loc)
A<-list.files(pattern='xlsx') %>% (function(u){split(u,u)}) %>%
lapply(function(u){readxl::read_excel(u,sheet='Resume') %>%
mutate(fl=u) %>%
filter(Tick!="Calibration") %>%
mutate(Well=factor(Well)) %>%
mutate(m=factor(c("m1","m2")[as.numeric(as.factor(Tick>3))])) %>%
(function(u){aggregate(formula=Counts~Well+Emission+m,data=u,FUN=mean)}) %>%
spread(m,Counts)%>%
mutate(delta=m1-m2,gain=delta/800,nrm.gain=(Emission/m1)*gain)
})%>% bind_rows

model<-lm(A$nrm.gain~A$Emission)
names(model$coef)<-c("pH_C:Intercept","pH_B:Slope")
write.csv(A,'barcodedata.csv',row.names=FALSE)
modelP<-c(names(model$coef)[1],model$coef[1],names(model$coef)[2],model$coef[2])
write(modelP,'coefficients.txt')
