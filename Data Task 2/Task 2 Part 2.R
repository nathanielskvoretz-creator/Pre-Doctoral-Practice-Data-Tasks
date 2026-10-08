library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
library(modelsummary)

ira_tweets <- read.csv("data/Part2/ira_tweets_csv_hashed.csv")

#Part 2
#a

tweet_data_day <- ira_tweets |> 
  mutate(date_time = ymd_hm(tweet_time), 
         date = as_date(date_time), 
         BLM_mention = as.numeric(str_detect(
           tweet_text, "(?i)\\b(blm|black\\s*lives\\s*matter|the\\s+movement\\s+for\\s+black\\s+lives)\\b"
           )
           )
         ) |> 
  group_by(date) |> 
  summarise(
    tweets = n(), 
    replies = sum(reply_count, na.rm = TRUE), 
    likes = sum(like_count, na.rm = TRUE), 
    quotes = sum(quote_count, na.rm = TRUE), 
    retweets = sum(retweet_count, na.rm = TRUE), 
    BLM = sum(BLM_mention, na.rm = TRUE)
  )

#b
plot_tweets <- tweet_data_day |> 
  ggplot(aes(x = date, y = tweets)) +
  geom_line() +
  xlim(as_date("2014-01-01"),NA)

plot_replies <- tweet_data_day |> 
  ggplot(aes(x = date, y = replies)) +
  geom_line() +
  xlim(as_date("2014-01-01"),NA)

plot_likes <- tweet_data_day |> 
  ggplot(aes(x = date, y = likes)) +
  geom_line() +
  xlim(as_date("2014-01-01"),NA)

plot_quotes <- tweet_data_day |> 
  ggplot(aes(x = date, y = quotes)) +
  geom_line() +
  xlim(as_date("2014-01-01"),NA)

plot_retweets <- tweet_data_day |> 
  ggplot(aes(x = date, y = retweets)) +
  geom_line() + 
  xlim(as_date("2014-01-01"),NA)

plot_BLM <- tweet_data_day |> 
  ggplot(aes(x = date, y = BLM)) +
  geom_line() + 
  xlim(as_date("2014-01-01"),NA)

(plot_tweets | plot_replies) / (plot_likes | plot_quotes) / (plot_retweets | plot_BLM) 

#c
tweet_event_window <- tweet_data_day |> 
  filter(between(date, as_date("2015-08-19") - days(30), as_date("2015-08-19") + days(30)) & date != as_date("2015-08-19")| 
         between(date, as_date("2015-07-13") - days(30), as_date("2015-07-13") + days(30)) & date != as_date("2015-07-13")| 
         between(date, as_date("2016-07-05") - days(30), as_date("2016-07-05") + days(30)) & date != as_date("2016-07-05")
         ) |> 
  mutate(
    Freddie_Grey = case_when(
      between(date, as_date("2015-08-19") - days(30), as_date("2015-08-19")) ~ 0,
      between(date, as_date("2015-08-19"), as_date("2015-08-19") + days(30))  ~ 1, 
      .default = NA
      ),
    Sandra_Bland = case_when(
      between(date, as_date("2015-07-13") - days(30), as_date("2015-07-13")) ~ 0,
      between(date, as_date("2015-07-13"), as_date("2015-07-13") + days(30))  ~ 1, 
      .default = NA
    ),
    Alon_Sterling = case_when(
      between(date, as_date("2016-07-05") - days(30), as_date("2016-07-05")) ~ 0,
      between(date, as_date("2016-07-05"), as_date("2016-07-05") + days(30))  ~ 1, 
      .default = NA
    )
  )

#regressions
#Freddie Grey
tweets_fg_model <- lm(data = tweet_event_window, tweets ~ Freddie_Grey)
replies_fg_model <- lm(data = tweet_event_window, replies ~ Freddie_Grey)
likes_fg_model <- lm(data = tweet_event_window, likes ~ Freddie_Grey)
quotes_fg_model <- lm(data = tweet_event_window, quotes ~ Freddie_Grey)
retweets_fg_model <- lm(data = tweet_event_window, retweets ~ Freddie_Grey)
BLM_fg_model <- lm(data = tweet_event_window, BLM ~ Freddie_Grey)

#Sandra Bland
tweets_sb_model <- lm(data = tweet_event_window, tweets ~ Sandra_Bland)
replies_sb_model <- lm(data = tweet_event_window, replies ~ Sandra_Bland)
likes_sb_model <- lm(data = tweet_event_window, likes ~ Sandra_Bland)
quotes_sb_model <- lm(data = tweet_event_window, quotes ~ Sandra_Bland)
retweets_sb_model <- lm(data = tweet_event_window, retweets ~ Sandra_Bland)
BLM_sb_model <- lm(data = tweet_event_window, BLM ~ Sandra_Bland)

#Alon Sterling
tweets_as_model <- lm(data = tweet_event_window, tweets ~ Alon_Sterling)
replies_as_model <- lm(data = tweet_event_window, replies ~ Alon_Sterling)
likes_as_model <- lm(data = tweet_event_window, likes ~ Alon_Sterling)
quotes_as_model <- lm(data = tweet_event_window, quotes ~ Alon_Sterling)
retweets_as_model <- lm(data = tweet_event_window, retweets ~ Alon_Sterling)
BLM_as_model <- lm(data = tweet_event_window, BLM ~ Alon_Sterling)

models <- list(
  "FGt" = tweets_fg_model, 
  "FGr" = replies_fg_model, 
  "FGl" = likes_fg_model,
  "FGq" = quotes_fg_model,
  "FGrt" = retweets_fg_model, 
  "FGb" = BLM_fg_model, 
  
  #Sandra Bland
  "SBt" = tweets_sb_model, 
  "SBr" = replies_sb_model, 
  "SBl" = likes_sb_model,
  "SBq" = quotes_sb_model,
  "SBrt" = retweets_sb_model, 
  "SBb" = BLM_sb_model, 
  
  #Alon Sterling
  "ASt" = tweets_as_model, 
  "ASr" = replies_as_model, 
  "ASl" = likes_as_model,
  "ASq" = quotes_as_model,
  "ASrt" = retweets_as_model, 
  "ASb" = BLM_as_model
)
modelsummary(models)

#d
# The interpretation of the coefficient is the average difference between the pre and post event tweets. 
# For example, the model indicates the number of tweets dropped by 163.7 tweets per day after Freddie Grey's death as compared to before.