#Einfach nur die network_auto() ausführen um direkt einen Df mit Screenname, InfluencerID und 
#FollowerID zu bekommen
network_auto <- function(influencer_csv, follower_csv){
  
  influencer <- influencer("user.csv")
  follower <- follower("user_follow.csv")
  network <- network(influencer, follower)
  
  return(network)
}

#Unternehmensnamen finden und Tweets finden, in denen
# a) der Unternehmensname vorkommt
# b) von einem Influencer getwittert wurde
influencer_tweets <- function(code){
  
  company_name <- keywords %>%
    filter(symbol == code) 
  company_name <- company_name[,-c(1,3,4)] %>%
    as.character()
  
  influencer_tweets <- tweets %>% 
    filter(str_detect(text, company_name))
  influencer_tweets <- influencer_tweets %>%
    mutate(influencer = influencer_tweets$userID %in% network$InfluencerID) %>%
    filter(influencer == TRUE)
  
  return(influencer_tweets)
}


#Aufbereitung der Daten
#_____Funktion zur Aufbereitung der Tweets 
tweets_df <- function(csv) {
  
  raw <- read.csv(csv) 
  raw$hastags <- as.character(raw$hastags)
  raw$text <- as.character(raw$text)
  raw$created_at <- as.POSIXct(raw$created_at)
  #raw$created_at <- as.character(name$created_at)
  #raw <- raw %>% 
  #  separate(created_at, c("date", "time"), sep = " ")
  
  return(raw)
}


#_____Funktion zur Aufbereitung der Financial Data
financial_df <- function(csv){
  
  raw <- read.csv(csv) 
  raw$timestamp <- as.POSIXct(raw$timestamp)
  raw$company <- as.character(raw$company)
  
  return(raw)
}


#_____Funktion zur Aufbereitung der Influencer Data
influencer <- function(csv){
  
  raw <- read.csv(csv)
  raw$screen_name <- as.character(raw$screen_name)
  
  return(raw) 
}

#_____Funktion zur Aufbereitung der Follower Data
follower <- function(csv){
  
  raw <- read.csv(csv)

  return(raw)  
}


#_____Influencer und Follower mergen
#Input: user() und follower()
network <- function(influencer, follower){
  
  influencer <- influencer %>%
  rename(userTargetID = userID)
  
  network <- left_join(influencer, follower) %>%
  rename(InfluencerID = userTargetID, FollowerID = userID)
  
  network <- network[,c(2,1,3)]

return(network)
}


#_____searchKeywords
searchKeywords <- function(csv){
  
  raw <- read.csv(csv)
  raw$content <- as.character(raw$content)
  raw$symbol <- as.character(raw$symbol)
  
  return(raw)
  
}


searchKeywords <- searchKeywords("searchKeywords.csv")

raw$processDate <- as.Date(raw$processDate)


influencer9 <- influencer("user.csv")
follower9 <- follower("user_follow.csv")
network9 <- network(influencer9, follower9)



influencer_list <- user_df("user.csv")
follower <- follower_df("user_follow.csv")


#tweets4 <- tweets_df("tweet.csv")
financial_data <- financial_df("Financial_Data.csv")
