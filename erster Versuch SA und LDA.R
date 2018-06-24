library(tidyverse)
library(tidytext)


# Funktion zur Aufbereitung der Tweets ------------------------------------

tweets_df <- function(csv, name) {
  name <- read.csv(csv, stringsAsFactors = FALSE)
  #name$hastags <- as.character(name$hastags)
  #name$text <- as.character(name$text)
  #name$created_at <- as.character(name$created_at)
  name <- name %>% separate(created_at, c("date", "time"), sep = " ")
}


# Tweet laden ---------------------------------------------
tweets <- tweets_df("tweet(2).csv", tweets)
#tweets %>% count(hastags, sort = T)
tweets[15,]

# URL's entfernen ---------------------------------------------------------
tweets$text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tweets$text)


# Tweets aufbereiten ------------------------------------------------------

text_df <- tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#text_df %>% count(word, sort = TRUE)
#Komische Wörter/Zeichen müssen noch entfernt werden


# Wörter nach Kategorien ordnen (nrc) -------------------------------------

sentiments_nrc <- text_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE)

ggplot(sentiments_nrc, aes(sentiment, n)) +
  geom_col()


# Wörter nach Kategorien ordnen (loughran) --------------------------------

sentiments_loughran <- text_df %>%
  inner_join(get_sentiments("loughran")) %>%
  count(sentiment, sort = TRUE) 

ggplot(sentiments_loughran, aes(sentiment, n)) +
  geom_col()




# Entwicklung des Sentiments im zeitlichen Verlauf für nrc, bing, afinn und loughran

 custom_nrc <- get_sentiments("nrc") %>%
   anti_join(data_frame(word = c("corporation", "share", "stocks", "expect"), lexicon = c("own", "own", "own", "own")))

# nrc-Wörterbuch
sentiments_nrc <- text_df %>%
  inner_join(custom_nrc) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort = T)
  group_by(date, sentiment) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "nrc")

# Bing-Wörterbuch
sentiments_bing <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  #count(word, sentiment, sort = T)
  group_by(date, sentiment) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing")
  
# afinn-Wörterbuch
sentiments_afinn <- text_df %>%
  inner_join(get_sentiments("afinn")) %>%
  #count(word, score, sort = T)
  group_by(date) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(lexicon = "afinn")

# loughran-Wörterbuch
sentiments_loughran <- text_df %>%
  inner_join(get_sentiments("loughran")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort = T)
  group_by(date,sentiment) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "loughran")

# Zusammenfassung der Sentiment-Analysen und Visualisierung ---------------

sentiments_merged <- bind_rows(sentiments_afinn, sentiments_bing, sentiments_nrc, sentiments_loughran)

ggplot(sentiments_merged, aes(date, sentiment, group = lexicon, color = lexicon)) +
  geom_line(size = 1) +
  labs(x = "Datum", y = "Sentiment", title = "Entwicklung des Sentiments im zeitlichen Verlauf")

# Idee: Aus den drei Wörterbüchern ein gemeinsames Gesamtsentiment erstellen (Gewichtung jeweils
#       33,3% pro Wörterbuch?)


# Topic Modeling: LDA -----------------------------------------------------

library(topicmodels)

own_stopwords <- bind_rows(data_frame(word = c("https", "t.co", "amp", "â", "ã", "æ'â", "å
                                               ", "sâ","ë"),
                                      lexicon = c("own", "own", "own","own", "own", "own","own", "own", "own")), stop_words)

clean_tweets <- tweets %>%
  unnest_tokens(word, text) %>%
  #anti_join(own_stopwords)
  unnest_tokens(stop_words)

tweets_dtm <- clean_tweets %>%
    count(tweetID, word) %>%
  cast_dtm(document = tweetID, term = word, value = n)

tweets_lda <- LDA(tweets_dtm, k = 20, control = list( seed = 1234))
tweets_topics <- tidy(tweets_lda, matrix = "beta")


# Häufigste Wörter pro Topic ----------------------------------------------

tweets_top_terms <- tweets_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tweets_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# bestes k

library("ldatuning")

result <- FindTopicsNumber(tweets_dtm,
                           topics = seq(from = 2, to = 10, by = 1),
                           metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                           method = "Gibbs",
                           control = list(seed = 77),
                           mc.cores = 2L,
                           verbose = TRUE)
FindTopicsNumber_plot(result)
