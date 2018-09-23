library(RMySQL)
library(DBI)
library(tidyverse)
library(tidytext)
library(lubridate)

# Funktion für leere Reihen
addcol <- function(input, colname) {
  newcol <-colname[!colname %in% names(input)]
  
  if(length(newcol)!=0) input[newcol] <- 0
  input
}

# Connection zur Datenbank aufbauen
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "web154_db19", 
                 host = 's92.goserver.host', 
                 port = 3306,
                 user = "web154_19",
                 password = "PJS2018!")

# News von Datenbank laden
bloomberg_raw <- dbReadTable(con, "Bloomberg_News") 

# News aufbereiten 
bloomberg_raw <- bloomberg_raw %>% 
  separate(Timestamp, c("date", "time"), sep = " ")

# Leeren Dataframe erstellen für das Sentiment jeder News
bloomberg_sentis <- data.frame(date=as.Date(character()),
                               sentiment=numeric(),
                               stringsAsFactors=FALSE) 

negation_words <- c("not", "no", "never", "without")

# Schleife, die für jede News eine neue Reihe mit Sentiment hinzufügt
for(i in 1:nrow(bloomberg_raw)){ 

  bloomberg_news <- bloomberg_raw[i,] 
  bloomberg_bigrams <- bloomberg_raw[i,]
  bloomberg_date <- bloomberg_news$date
  
  bloomberg_news <- bloomberg_news %>%
    unnest_tokens(word, Content) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("loughran")) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    group_by(date,sentiment) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0)
  
  #Code gibt Fehler aus, wenn es weder ein positives, noch ein negatives Wort gibt. Daher muss überprüft werden, ob der Dataframe aus 0 Rows besteht, und wenn ja, müssen die fehlenden Spalten "date", "positive" und "negative" erstellt werden
  #Die erstellten Spalten dann anschließend mit dem Datum und jeweils 0 fürs Sentiment befüllen
  if(nrow(bloomberg_news) < 1) {
    bloomberg_news <- add_column(bloomberg_news, positive = NA , negative = NA)
    bloomberg_news = as.data.frame(list(NA, 0, 0)) %>%
      rename(date = NA., positive = X0, negative = X0.1) 
    bloomberg_news$date <- bloomberg_date 
  } 
  else { # Wenn es entweder nur positive/negative Wörter gibt, sind nur die Spalten definiert, die einen Wert enthalten. Deswegen muss man hier die leeren Spalten erstellen, damit man im nächsten Schritt die Berechnung "sentiment = positive - negative" durchführen kann
    bloomberg_news <- addcol(bloomberg_news, "positive") 
    bloomberg_news <- addcol(bloomberg_news, "negative")
  }
  
  bloomberg_news <- bloomberg_news %>% 
    ungroup() %>%
    mutate(sentiment = positive - negative) %>%
    mutate(lexicon = "loughran") 
  
# Sentiment für Bigrams berechnen  
  bigrams <- bloomberg_bigrams %>%
    unnest_tokens(bigram, Content, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% negation_words) %>%
    #filter(word1 == "not") %>%
    filter(!word2 %in% stop_words$word) %>%
    inner_join(get_sentiments("loughran"), by = c(word2 = "word")) %>%
    filter(sentiment == "positive" | sentiment == "negative") %>%
    mutate(bigram_senti = ifelse(sentiment == "positive", "negative", "positive")) %>%
    group_by(date, bigram_senti) %>%
    count() %>%
    ungroup() %>%
    mutate(senti_switch = ifelse(bigram_senti == "positive", n * 2, n * -2)) %>%
    group_by(date) %>%
    summarise(senti_switch = sum(senti_switch))
  
  #hier wieder eine Zeile mit senti_switch = 0 auffüllen, sonst kann später nicht sentiment + senti_switch gerechnet werden
  if(nrow(bigrams) < 1) {
    bigrams = as.data.frame(list(NA, 0)) %>%
      rename(date = NA., senti_switch = X0) 
    bigrams$date <- bloomberg_date 
  }   

# Word und Bigram Sentiment joinen und das finale Sentiment bestimmen 
senti_joined <- full_join(bloomberg_news, bigrams) %>%
  mutate(sentiment_final = sentiment + senti_switch)

# Finales Sentiment zur Dataframe hinzufügen
  bloomberg_sentis <- add_row(bloomberg_sentis, date = senti_joined$date, sentiment = senti_joined$sentiment_final)
  bloomberg_news <- bloomberg_raw
  
remove(bigrams, senti_joined)
}

# Senti-Score bestimmen
bloomberg_scores <- bloomberg_sentis %>%
  mutate(type = ifelse(sentiment > 0, "POS", "NEG")) %>%
  group_by(date) %>%
  count(type) %>%
  spread(type, n, fill = 0) %>%
  mutate(senti_score = POS / (POS + NEG)) %>%
  mutate(volume = POS + NEG) %>%
  select(-NEG, -POS)
# _

# Fehlende Datums hinzufügen
bloomberg_scores$date <- as.Date(bloomberg_scores$date)
start_date <- min(bloomberg_scores$date)
end_date <- max(bloomberg_scores$date)

dates_full <- seq(ymd(start_date), ymd(today()), by="day") %>% #hier jetzt mit today() anstat end_date, weil News von Chen fehlen
  as.data.frame() %>% 
  rename(date = '.')

bloomberg_scores <- full_join(bloomberg_scores, dates_full) %>%
  complete(date, fill = list(volume = 0, senti_score = 0.5)) %>%
  ungroup() %>%
  mutate(senti_score2 = lag(senti_score, default = 0.5)) %>% 
  mutate(senti_score3 = lag(senti_score, n = 2L, default = 0.5))

remove(dates_full)         

#Sentis fürs ML
#sentis_ml <- bloomberg_scores %>% 
  #filter(date >= "2018-07-01" & date <= "2018-08-15") #%>%
 # write.csv(file = "sentis_ml.csv", row.names = FALSE)


#Unnötige Daten löschen für die Übersicht
#remove(bloomberg_news)
#remove(bloomberg_raw)
#remove(bloomberg_scores)
#remove(bloomberg_sentis)
#remove(con)
#rm(list = ls.str(mode = 'character'))
#rm(list = ls.str(mode = 'numeric'))
