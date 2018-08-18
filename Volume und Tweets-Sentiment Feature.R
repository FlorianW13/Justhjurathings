library(RMySQL)
library(DBI)
library(tidyverse)
library(tidytext)

# Connection zur Datenbank aufbauen
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "", 
                 host = '', 
                 port = ,
                 user = "",
                 password = "")

#Tweets von Datenbank laden
tweets <- dbReadTable(con, "tweet")

# Tweets aufbereiten ------------------------------------------------------
tweets_clean <- tweets %>% separate(created_at, c("date", "time"), sep = " ")
text_df <- tweets_clean %>%
  unnest_tokens(word, text)

text_df$searchKeywordID <- as.factor(text_df$searchKeywordID)
text_df$date <- as.factor(text_df$date)

# Senti-Scores finito
sentiment_scores_with_lag <- text_df %>%
  filter(!is.na(searchKeywordID)) %>%
  inner_join(get_sentiments("loughran")) %>%
  group_by(date, tweetID, searchKeywordID, sentiment) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  mutate(type = ifelse(sentiment > 0, "POS", "NEG")) %>%
  group_by(date, searchKeywordID) %>%
  count(type) %>%
  spread(type, n, fill = 0) %>%
  mutate(senti_score = POS / (POS + NEG)) %>%
  mutate(volume = POS + NEG) %>%
  group_by(searchKeywordID) %>%
  complete(date, nesting(searchKeywordID), fill = list(volume = 0, senti_score = 0.5)) %>%
  select(-NEG, -POS) %>%
  group_by(searchKeywordID) %>%
  mutate(senti_score2 = lag(senti_score)) %>%
  mutate(senti_score3 = lag(senti_score, n = 2L)) %>%
  mutate(senti_score2 = ifelse(is.na(senti_score2), 0.5, senti_score2)) %>%
  mutate(senti_score3 = ifelse(is.na(senti_score3), 0.5, senti_score3))
