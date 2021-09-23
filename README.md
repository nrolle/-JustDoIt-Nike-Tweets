# (Project Overview: Text Analysis)(https://nrolle.github.io/-JustDoIt-Nike-Tweets/)

- Wrangling and Visualizing Text using the Tokenization method 
- Discovering the emotional valence of the text using Sentiment Analysis
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

![image](https://user-images.githubusercontent.com/90916159/134436474-388dc60b-6a4d-4708-8299-6adbf623db34.png)


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
## Removing blank row from Summary_user_verified 
Summary_user_verified2 = Summary_user_verified[-c(1),]

## Summary of Users who are Replying to Trump 
SummaryTrump = Tweets2 %>% 
  filter(tweet_in_reply_to_screen_name == "realDonaldTrump") %>% 
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

## Summary of Users who are Replying to Colin Kaepernick7 
SummaryKaep = Tweets2 %>% 
  filter(tweet_in_reply_to_screen_name == "Kaepernick7") %>% 
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
 
## Summary of Users who are Replying to Nike
SummaryNike = Tweets2 %>% 
  filter(tweet_in_reply_to_screen_name == "Nike") %>% 
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
 
## Tweets with the Most Favorites! 
TopFav = Tweets2 %>% 
  top_n(10, tweet_favorite_count) %>% 
  select(tweet_favorite_count,tweet_full_text, user_screen_name, user_verified) %>% 
  arrange(desc(user_verified)) 
 
## Tweets with the Most Retweets!
TopRT = Tweets2 %>% 
  top_n(10, tweet_retweet_count) %>% 
  select(tweet_retweet_count, tweet_full_text, user_screen_name, user_verified) %>% 
  arrange(desc(user_verified)) 
 
## Tokenizing the Text - Making a Word Column!
tidy_twitter = Tweets %>% 
  unnest_tokens(word,tweet_full_text) 

## Adding A New Dataframe called custom_stop_words 
custom_stop_words = tribble(~word, "t.co", "https", "it's","means", "i'm")

## Combining custom_stop_words to the data frame stop_words
stop_words2 = stop_words %>% 
  bind_rows(custom_stop_words)
  
## Counting the Tokenized Text - Removing stop_words from text - Arranging the data frame in descending order
tidy_twitter2 = tidy_twitter %>% 
  count(word) %>% 
  anti_join(stop_words2) %>% 
  arrange(desc(n))
 
## Filtering out any words less than 2 tokens
word_count = tidy_twitter2 %>% 
  filter(n>1)

## Showing the Top 30 Words Used in Response to the Original Tweet - Reordering by the Word Count (n)
word_count2 = word_count %>% 
  top_n(30) %>% 
  mutate(word2 = fct_reorder(word, n))

## Visualization of Word Count
word_countplot2 = ggplot(word_count2, aes(x = word2, y = n, fill = n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Keywords Used in Response to the Ad Campaign",
       subtitle = "Sample Size: 5,000 Tweets That Contain '#JustDoIt'",
       x = "Word",
       y = "Number of Occurrences")

![image](https://user-images.githubusercontent.com/90916159/134451292-f76a0d48-71f2-408e-ac88-300f991b9bd7.png)

## Word Count for Top 10 Most Favorited Tweets
TopFav_word = TopFav %>% 
  unnest_tokens(word, tweet_full_text)

## TopFav Word Count
TopFav_wordcount = TopFav_word %>% 
  count(word) %>% 
  mutate(FavWord = fct_reorder(word, n)) %>% 
  anti_join(stop_words2) %>% 
  top_n(30,n)

## Visualization of Tokenized Text in the Top 10 Favorited Tweets 
TopFav_wordcountplot = ggplot(TopFav_wordcount, aes(x=FavWord, y=n, fill=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Keywords Used in Response to the Ad Campaign",
       subtitle = "Sample Size: Top 10 Favorited Tweets",
       x = "Word",
       y = "Number of Occurrences")

![image](https://user-images.githubusercontent.com/90916159/134451340-0cc33d9d-322e-42df-8c5c-4215dbf60292.png)

## Word Count for Top 10 Most Retweeted Tweets
TopRT_word = TopRT %>% 
  unnest_tokens(word, tweet_full_text)

TopRT_wordcount = TopRT_word %>% 
  count(word) %>% 
  mutate(RTWord = fct_reorder(word,n)) %>% 
  anti_join(stop_words2) %>% 
  top_n(30, n)
  
## Visualization of Tokenized Text in the Top 10 Favorited Retweets 
TopRT_wordcountplot = ggplot(TopRT_wordcount, aes(x=RTWord, y=n, fill=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Keywords Used in Response to the Ad Campaign",
       subtitle = "Sample Size: Top 10 Retweeted Tweets",
       x = "Word",
       y = "Number of Occurrences")

![image](https://user-images.githubusercontent.com/90916159/134451358-554704cb-1586-48b5-8d07-19b33e356d19.png)

## Appending Dictionaries - bing
Append_twitter = tidy_twitter %>% 
  inner_join(get_sentiments("bing"))

## Counting Sentiment
Count_twitter_sentiment = Append_twitter %>% 
  count(word, sentiment) %>% 
  arrange(desc(n))
 
## Visualizing Sentiment
sentiment_twitter = Count_twitter_sentiment %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  top_n(55, n)
  
ggplot(sentiment_twitter, aes(x=word, y=n, fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  labs(title = "Sentiment Word Counts", x = "Words")
  
 ![image](https://user-images.githubusercontent.com/90916159/134451405-969849b7-2ac6-4478-8ab7-e42fea821fe1.png)

## Using spread() to spread the sentiment column in to two separate columns (e.g., pos or neg)
xyz = Append_twitter %>% 
  count(tweet_in_reply_to_screen_name, sentiment) %>% 
  spread(sentiment, n)
  
## Topic Models 
DTM = tidy_twitter2 %>% 
  count(word, id) %>% 
  cast_dtm(id, word, n)

lda_out = LDA(DTM, k = 2, method = "Gibbs", control = list(seed=42))

glimpse(lda_out)

tidy(lda_out)




