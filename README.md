# (Project Overview: Text Analysis)(https://nrolle.github.io/-JustDoIt-Nike-Tweets/)

- How to Wrangle and Visualize Text 
- Perform Sentiment Analysis
- Run and Interpret Topic Models


## Code and Resources Used
**RStudio Version:** 1.4.1103 
**Packages:** tidyverse, dplyr, tibble, tidytext, textdata, topicmodels, tm
Dataset(https://www.kaggle.com/eliasdabbas/5000-justdoit-tweets-dataset?select=justdoit_tweets_2018_09_07_2.csv)

## Setting up the R environment - Loading Libraries 
library(tidyverse)
library(dplyr)
library(tibble)
library(tidytext)
library(textdata)
library(topicmodels)
library(tm)

## Read in File 
Tweet = read.csv("/Users/nickrolle/Downloads/justdoit_tweets_2018_09_07_2.csv")

## Dimension of input file
nrow(Tweet)
ncol(Tweet)
dim(Tweet)
str(Tweet)

## Sample Tweets Text
head(Tweet$tweet_full_text)

## Removing Columns Insignificant to Our Analysis
Tweets = Tweet %>% 
  select(-tweet_contributors) %>% 
  select(-tweet_geo) %>% 
  select(-tweet_id) %>% 
  select(-tweet_id_str) %>% 
  select(-tweet_in_reply_to_status_id_str) %>% 
  select(-tweet_quoted_status) %>% 
  select(-tweet_source) %>% 
  select(-user_description) %>% 
  select(-tweet_coordinates) %>% 
  select(-tweet_favorited) %>% 
  select(-tweet_in_reply_to_status_id) %>% 
  select(-tweet_quoted_status_id) %>% 
  select(-tweet_quoted_status_id_str) %>% 
  select(-tweet_retweeted) %>% 
  select(-user_created_at) %>% 
  select(-user_profile_background_image_url)

## Removing the Mark Hamill Tweet 
Tweets2 = Tweets[-c(1466),]

# Top Screen Names Receiving Replies
Replies = Tweets2 %>% 
  select(tweet_in_reply_to_screen_name) %>% 
  count(tweet_in_reply_to_screen_name) %>% 
  arrange(desc(tweet_in_reply_to_screen_name)) %>% 
  top_n(10)
  
# Removing the Blank Row in Replies 
Replies_2 = Replies[-c(12),]

# Fct_reorder - Cleaning up the Graph
Replies_3 = Replies_2 %>% 
  mutate(n2 = fct_reorder(tweet_in_reply_to_screen_name, n))
  
# Visualization of Replies_2 - Removal of "Blank Spaces" - Top 11 @'s Replied To
Replies_plot = ggplot(data=Replies_3, aes(x=n2, y=n, fill = n))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Tweets in Reply To...")+
  xlab("Twitter Handle")+
  ylab("Number of Replies")

# Summary of Verified Users vs Unverified Users - Favorites / Retweets / Followers
Summary_user_verified = Tweets2 %>% 
  group_by(user_verified) %>% 
  summarize(avg_favorites = mean(tweet_favorite_count),
            min_favorites = min(tweet_favorite_count),
            max_favorites = max(tweet_favorite_count),
            avg_retweets = mean(tweet_retweet_count),
            min_retweets = min(tweet_retweet_count),
            max_retweets = max(tweet_retweet_count),
            avg_followers = mean(user_followers_count),
            min_followers = min(user_followers_count),
            max_followers = max(user_followers_count)
            )
