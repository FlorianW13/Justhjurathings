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


#_____Funktion zur Aufbereitung der User Data
user_df <- function(csv){
  
  raw <- read.csv(csv)
  raw$screen_name <- as.character(raw$screen_name)
  
  return(raw) 
}






#tweets4 <- tweets_df("tweet.csv")
#financial_test <- financial_df("Financial_Data.csv")
#ausers <- user_df("user.csv")