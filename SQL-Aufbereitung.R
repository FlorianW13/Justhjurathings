#Tweets aufbereiten
tweets_fun <- function() {
  
  library(RMySQL)
  mydb1= dbConnect(MySQL(), user='web154_19', password='PJS2018!', dbname='web154_db19', host='s92.goserver.host')
  sql_query <- paste0("SELECT * FROM `tweet`")
  rs <- dbSendQuery(mydb1, sql_query)
  raw = fetch(rs, n=-1)
  
  raw$created_at <- as.POSIXct(raw$created_at)
  #  separate(created_at, c("date", "time"), sep = " ")
  
  return(raw)
}

#Financial Data aufbereiten
financial_data_fun <- function(){
  
  library(RMySQL)
  mydb1= dbConnect(MySQL(), user='web154_19', password='PJS2018!', dbname='web154_db19', host='s92.goserver.host')
  sql_query <- paste0("SELECT * FROM `Financial_Data`")
  rs <- dbSendQuery(mydb1, sql_query)
  raw = fetch(rs, n=-1)
  
  raw$timestamp <- as.POSIXct(raw$timestamp)
  
  return(raw)
}

#searchKeywords aufbreiten
searchKeywords_fun <- function(){
  
  library(RMySQL)
  mydb1= dbConnect(MySQL(), user='web154_19', password='PJS2018!', dbname='web154_db19', host='s92.goserver.host')
  sql_query <- paste0("SELECT * FROM `searchKeywords`")
  rs <- dbSendQuery(mydb1, sql_query)
  raw = fetch(rs, n=-1)
  
  raw$processDate <- as.POSIXct(raw$processDate)
  
  return(raw)
}



#Diese Funktion fürs Netzwerk benutzen
network_alluser_fun <- function(){
  
  influencer <- influencer_fun()
  follower <- follower_fun()
  network <- network_fun(influencer, follower)
  
  return(network)
}

#Input für network_alluser_fun() 
influencer_fun <- function(){
  
  library(RMySQL)
  mydb1= dbConnect(MySQL(), user='web154_19', password='PJS2018!', dbname='web154_db19', host='s92.goserver.host')
  sql_query <- paste0("SELECT * FROM `user`")
  rs <- dbSendQuery(mydb1, sql_query)
  raw = fetch(rs, n=-1)
  
  raw$userID <- as.character(raw$userID)
  
  return(raw)
}
follower_fun <- function(){ 
  
  library(RMySQL)
  mydb1= dbConnect(MySQL(), user='web154_19', password='PJS2018!', dbname='web154_db19', host='s92.goserver.host')
  sql_query <- paste0("SELECT * FROM `user_follow`")
  rs <- dbSendQuery(mydb1, sql_query)
  raw = fetch(rs, n=-1)
  
  return(raw)
}
network_fun <- function(influencer, follower){
  
  influencer <- influencer %>%
    rename(userTargetID = userID)
  
  network <- left_join(influencer, follower) %>%
    rename(InfluencerID = userTargetID, FollowerID = userID)
  
  #network <- network[,c(2,1,3)]
  
  return(network)
}








#Wenn max. Anzahl an Verbindungen erreicht ist:
#lapply(dbListConnections(MySQL()), dbDisconnect)