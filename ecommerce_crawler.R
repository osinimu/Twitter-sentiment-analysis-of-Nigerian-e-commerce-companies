library(twitteR)
library(RCurl)
library(httr)
library(dplyr)
library(base64enc)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)


api_key <- "TS0SXHklfA6hAUWKJXFqB7yjI"
api_secret <- "xuy7ApQ7j9nM8Nh3GscBJBK9sNYFMMLK8JyrXHcOdbvGhmexyW"
token <- "87423467-PYCD663M7AWCJXKmLHtCb30HA9baxzaTq6KsLcTQD"
token_secret <- "fRMMrGquliERpmDEuV7uYgwbiVUakKrvqriITdyDBP2pr"

#create a twitter connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)
token <- get("oauth_token", twitteR:::oauth_cache)
token$cache()
set_config(config (ssl_verifypeer= 0L))

#timeframe of tweets to be collected
date <- Sys.Date()
before_date <-as.character.Date(date-20)
after_date <- as.character.Date(date)

#konga list
tweetdetails <- searchTwitter("konga -from:shopkonga", n=3000, lang="en", since = before_date,until= after_date, geocode = '10,9, 200km') 
konga = twListToDF(tweetdetails)
konga$eCommerce_company = "konga"
kongaFinal = subset(konga,grepl('konga',text,ignore.case = T))

#Jumia
tweetdetails <- searchTwitter("jumia -from:jumiaonline", n=3000, lang="en", since = before_date,until= after_date, geocode = '10,9, 200km') 
jumia = twListToDF(tweetdetails)
jumia$eCommerce_company = "jumia"
jumiaFinal = subset(jumia,grepl('jumia',text,ignore.case = T))

#Payporte
tweetdetails <- searchTwitter("payporte -from:payporte", n=3000, lang="en", since = before_date,until= after_date, geocode = '10,9, 200km') 
payporte = twListToDF(tweetdetails)
payporte$eCommerce_company = "payporte"
payporteFinal = subset(payporte,grepl('payporte',text,ignore.case = T))

#Yudala
tweetdetails <- searchTwitter("Yudala -from:yudalaonline", n=3000, lang="en", since = before_date,until= after_date, geocode = '10,9, 200km') 
yudala = twListToDF(tweetdetails)
yudala$eCommerce_company = "yudala"
yudalaFinal = subset(yudala,grepl('yudala',text,ignore.case = T))

#combine al the data frames together
final_file <- rbind(kongaFinal, jumiaFinal, payporteFinal, yudalaFinal)


