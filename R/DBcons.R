# connect to RMySQL
rmysqlCon<-function(){
  require(RMySQL)
  ConnectInfo<-DataStash::Triton()  
  dbConnect(RMySQL::MySQL(),
            dbname=ConnectInfo[1],
            user=ConnectInfo[2],
            password=ConnectInfo[3],
            host=ConnectInfo[4],
            port=as.numeric(ConnectInfo[5]))
}


dplyrCON<-function(){ 
  require(dplyr)
  ConnectInfo<-DataStash::Triton() 
  src_mysql(ConnectInfo['dbname'],
                    user=ConnectInfo['user'],
                    password=ConnectInfo['password'],
                    host=ConnectInfo['host'],
                    port=as.numeric(ConnectInfo['port']))
}
