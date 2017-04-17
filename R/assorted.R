
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
  n<-length(list.files(path=path_in,pattern='.asyr'))
  if (n>0){
  PipeFish::exportXLS(path_in);
  PipeFish::XLSXos(file.path(path_in,"export"))
  }
}


torquemada<-function(Dir){
shell(paste0('torquemada',' "',Dir,'" -s'))
}



################
#PipeFishGUI<-function(){shell.exec(system.file(path='/gui/PipeFish.exe',package='PipeFish'))}


#LNK_OL <-function(){
#shell(system.file(package="PipeFish",path='vbs/Outlierlnk.vbs'))
#}

LNK_OL<-function(){ScriptCut("Outlier","PipeOL.R")}

available_scripts<-function(fullnames=F){
list.files(path=system.file(package="PipeFish",path='scripts'),full.names=fullnames)
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

 ## FIX for 2.3 Excel Files
fndLVLs<-function(u){
  if (any(grepl('Level',readxl::excel_sheets(u)))){
    'Level'
  }else{'Raw'}
}

##### fix Shiny file Input
 fixShinyFileInput<-function(InputFile){
    dir<-dirname(InputFile$datapath)
    bn<-basename(InputFile$datapath)
    fn<-InputFile$name
    fixedDP<-file.path(dir,fn)
    file.rename(InputFile$datapath,fixedDP)
    InputFile$datapath<-fixedDP
    InputFile
  }

######

.ky<-function(){"UzNKaGEyVnVPM0J2YzJWcFpHOXVPM0J2YzJWcFpHOXVPMjFoWjI1cGVDNXNhM0V1WVdkcGJHVnVkQzVqYjIwN016TXdOaVVsWkdKdVlXMWxPM1Z6WlhJN2NHRnpjM2R2Y21RN2FHOXpkRHR3YjNKMA=="}


.sharpen<-function(x){
  require(magrittr)
  base64enc::base64decode(x) %>%
    rawToChar() %>%
    {
      if (grepl('%%', .)) {
        strsplit(., split = "%%") %>%
          unlist() %>%
          lapply(., strsplit, split = ";") %>%
          lapply(unlist) %>%
          {
            setNames(.[[1]], .[[2]])
          }
      } else{
        .
      }
    }
}
