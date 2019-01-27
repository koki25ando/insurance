# Tweet analysis

# devtools::install_github("mkearney/rtweet")
library(rtweet)
library(httr)

# packageVersion("rtweet")

appname <- "Nike_Tweets_Analysis"
key <- "hIE9CVVsHFsGPWHsvjj9vc52X"
secret <- "I3CvypiOgfyjck9DDn0RsOKvJAUveAS3krFOK9ShLscd4iVie1"
at <- "2183834275-Tdz35pNu5m4SLJDLeqSQgLNixCGbnGpwTWYprV7"
as <- "2ymbDG538YprpMCQgoRJQYo1xLIydZixEAABlze7YFNEV"

twitter_token <- rtweet::create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = at, 
  access_secret = as)

zozo_tweets <- search_tweets(q = "zozotown", n = 20000) #テキストに"zozotown"を含むツイートを20,000個抽出
zozo_tweets %>% 
  head(20)

unique_zozo <- zozo_tweets[!duplicated(zozo_tweets$screen_name),] #重複したユーザーを削除 10,334行のデータまで絞り込む

# Export


# 