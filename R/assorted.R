
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
          normalizePath(path_in),
          normalizePath(newpath),
          sep=" "
          )
          )
}

## combines exporter and open save of asyr files
Outandsave<-function(path_in){
    PipeFish::exportXLS(path_in);
    PipeFish::XLSXos(file.path(path_in,"export"))
}

################
#PipeFishGUI<-function(){shell.exec(system.file(path='/gui/PipeFish.exe',package='PipeFish'))}


#LNK_OL <-function(){
#shell(system.file(package="PipeFish",path='vbs/Outlierlnk.vbs'))
#}

LNK_OL<-function(){ScriptCut("Outlier","PipeOL.R")}

available_scripts<-function(){
list.files(path=system.file(package="PipeFish",path='scripts'))
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
