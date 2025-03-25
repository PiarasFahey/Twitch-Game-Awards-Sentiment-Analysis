library(tidyverse)
library(tidytext)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(tm)
library(dplyr)
library(gganimate)
library(ggthemes)

# Data scraped from Twitch VOD
chat.data <- read.csv("GameAwardsfixed.csv")

chat.data$Time <- as.POSIXct(chat.data$Time, format="%H:%M:%S")

chat.words <- chat.data %>%
  select(Time, Username, Message) %>%
  unnest_tokens(word, Message)

# sentiment over time in 1 minute increments

chat.sentiments.text <- chat.words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(time_group = cut((hour(Time) * 60) + minute(Time),
                            breaks = seq(0, 452, by = 1))) %>%
  summarise(total.sentiment = sum(value))


# worth looking at this as it shows the words most contributing to the sentiment value
chat.sentiments.text.1 <- chat.words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarise(total.sentiment = sum(value))


# adjusting to half as the livestream is repeated in the twitch stream
chat.sentiments.text <- chat.sentiments.text[1:210,]

# sentiment analysis for 1 minute increments of just text
theme_set(theme_solarized())
ggplot(chat.sentiments.text, aes(x = 12:221, y = total.sentiment)) +
  geom_line(color = "#BF40BF", linewidth = 1) +
  labs(title = "Sentiment Over Time (Text)",
       y = "Total Sentiment Score",
       x = "Time period (Minutes)" )


# building emote lexicon

emotes <- read.csv("TwitchEmotes.csv", col.names = c("word", "value", "explanation"))

emotes <- emotes %>%
  select(word, value) %>%
  mutate(word = tolower(word))

chat_data_sentiment <- chat.data %>%
  select(Time, Username, Message) %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  left_join(emotes, by = "word")


# time series for sentiment analysis for 1 minute increments

chat_data_sentiment_summary.1 <- chat_data_sentiment %>%
  group_by(Time, Username, word) %>%
  summarize(sentiment_score = sum(value, na.rm = TRUE))

chat_data_sentiment_summary.1 <- chat_data_sentiment_summary.1 %>%
  mutate(interval = floor_date(Time, "1 minute")) %>%
  group_by(interval) %>%
  summarize(total_value = sum(sentiment_score))

chat_data_sentiment_summary.1 <- chat_data_sentiment_summary.1[1:210,]

# sentiment analysis for 1 minute increments of just Emoticons
ggplot(chat_data_sentiment_summary.1, aes(x = 12:221, y = total_value)) +
  geom_line(color = "#BF40BF", linewidth = 1) +
  labs(title = "Sentiment Over Time (Emotes)",
       y = "Total Sentiment Score",
       x = "Time period (Minutes)" )


# total sentiment for 1 minute increments

total.sentiment <- as.data.frame(c(chat_data_sentiment_summary.1$total_value)+c(chat.sentiments.text$total.sentiment))
total.sentiment <- as.data.frame(cbind(c(12:221), total.sentiment))
colnames(total.sentiment)[1]="time"
colnames(total.sentiment)[2]="value"

# sentiment analysis for 1 minute increments of Emoticons and Text
ggplot(total.sentiment, aes(time, y = total.sentiment[,2])) +
  geom_line(color = "#BF40BF", linewidth = 1) +
  labs(title = "Sentiment Over Time (Emotes and Text combined)",
       y = "Total Sentiment Score",
       x = "Time period (Minutes)" )
