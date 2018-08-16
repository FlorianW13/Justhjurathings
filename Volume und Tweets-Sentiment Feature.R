library(RMySQL)
library(DBI)
library(tidyverse)
library(tidytext)

# Connection zur Datenbank aufbauen
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "", 
                 host = '', 
                 port = 3306,
                 user = "",
                 password = "")

#Tweets von Datenbank laden
tweets <- dbReadTable(con, "tweet")

# Tweets aufbereiten ------------------------------------------------------
tweets_clean <- tweets %>% separate(created_at, c("date", "time"), sep = " ")
text_df <- tweets_clean %>%
  unnest_tokens(word, text)

# Sentiment-Feature mit nur einem pos/neg-Feature -------------------------

sentiments_bing <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(date, tweetID, searchKeywordID, sentiment) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  mutate(type = ifelse(sentiment > 0, "POS", "NEG")) %>%
  group_by(date, searchKeywordID) %>%
  count(type) %>%
  spread(type, n, fill = 0) %>%
  mutate(ratio_pos = POS / (POS + NEG)) %>%
  mutate(volume = POS + NEG)
