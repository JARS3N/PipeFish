
checkforpackage<-function(A){y<-installed.packages()[,1];chk<-(A %in% y);if (chk==FALSE){install.packages(A,repos='http://cran.us.r-project.org',dependencies=TRUE) }}

#############

XLSXos<-function(u=getwd()){
system(paste0(
"Cscript",
" ",
shQuote(normalizePath(system.file("/vbs/XLOSC.vbs", package="PipeFish"))),
" ",
shQuote(normalizePath(u))
))
}
## assuming MassAssayExporter.exe exists,is in the correct location and added to the system path
exportXLS<-function(path_in){
     newpath<-file.path(path_in,"export")
    if (dir.exists(newpath)==FALSE){dir.create(newpath)}
   shell(
       paste('MassAssayExporter.exe',
          shQuote(normalizePath(path_in)),
          shQuote(normalizePath(newpath)),
          sep=" "
          )
          )
}

## combines exporter and open save of asyr files
Outandsave<-function(path_in=getwd()){
    PipeFish::exportXLS(path_in);
    PipeFish::XLSXos(file.path(path_in,"export"))
}

################
#PipeFishGUI<-function(){shell.exec(system.file(path='/gui/PipeFish.exe',package='PipeFish'))}


#LNK_OL <-function(){
#shell(system.file(package="PipeFish",path='vbs/Outlierlnk.vbs'))
#}

LNK_OL<-function(){ScriptCut("Outlier","PipeOL.R")}

available_scripts<-function(fullnames=F){
list.files(path=system.file(package="PipeFish",path='scripts',full.names=F))
}


ScriptCut <-function(LinkName,ScriptName){
  LNKcreateVBS <- system.file(package="PipeFish",path="vbs/LNKcreate.vbs")
  shell(paste("Cscript //B ",shQuote(LNKcreateVBS),shQuote(LinkName),shQuote(ScriptName),sep=" ")) 
}
# for adding scripts Ad-hoc
StoreScript<-function(FROM_=file.choose()){
TO_ <-system.file(package="PipeFish",path='scripts');
file.copy(from=FROM_,to=TO_)
}


givename<-function(u,splits=": "){
  Q <-strsplit(u,split=splits);  
  out<-Q[[1]][2];
  names(out)<-Q[[1]][1];
  out
}

autoUpGithub<-function(pack){
 DESCfile<- system.file(package=pack,"DESCRIPTION")
 DESC<- readLines(DESCfile)
 LAST<-unlist(lapply(DESC,givename))
 pkg<-gsub("https://github.com/","",LAST["URL"])
 GETS<-paste0("https://raw.githubusercontent.com/",pkg,"/master/DESCRIPTION")
 ongit<- gsub("Version: ","",grep("Version: ",readLines(GETS),value=T))
 if (ongit!=LAST["Version"]){
   devtools::install_github(pkg)
 }
}
