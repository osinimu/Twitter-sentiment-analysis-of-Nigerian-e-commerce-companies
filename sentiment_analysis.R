11library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(tidytext)



#Ensuring the text is in the native R encoding
final_file$text = enc2native(final_file$text)

#Remove all the U+00 and leave the R encoding only
final_file$text = tolower(str_replace_all(final_file$text,"U\\+00",''))

#Because Emoji's show sentiment, they would be affording us a lot of information right?
#Thanks to Jessica Peterka-Bonetta! for compiling a list of emojis bytes and utf-8 characters and their descriptions.

emojis = read.csv("https://raw.githubusercontent.com/today-is-a-good-day/Emoticons/master/emDict.csv",sep = ";")
emojis$Native = NULL

#The function below find the first emoji in a tweet and puts it in a new column.
#The idea behind picking only one emoji comes from the assumption that one emoji out of the many that people put in a tweet is enough to find the sentiment of that tweet
#In fact, A lot of the time emojis are just repitions or are conveying the same emotion
extract_emojis = function(df){
  df$emoticon = ''
  pt = txtProgressBar(min = 0, max=nrow(df),initial = 0)
  for (i in 1:nrow(df)){
    if (str_count(df$text[i],"<e") >0){
      emoji_placeholder = "<e"
      k = str_locate(df$text[i],"<e")[2]
      while (emoji_placeholder %in% emojis$R.encoding == F){
        emoji_placeholder = substr(df$text[i],start = str_locate(df$text[i],"<e")[1],  stop = k)
        k=k+1
        if (k > nchar(df$text[i])){
          break
        }
      }
      
      df$emoticon[i] = emoji_placeholder
    }
    
    setTxtProgressBar(pt,i)
  }
  
  return (df)
}

final_file <- extract_emojis(final_file)

#Removing trailing and leading white space
final_file$emoticon <- gsub("^\\s+|\\s+$", "",final_file$emoticon)

#This function removes everything after the last occurrence of ">" in the emoticon column
final_file$emoticon=str_replace(final_file$emoticon,"(?<=>)[^>]*$",'')

#Get the description of the emoticons by merging the emoticons table with the tweets dataframe, final_file
final_file <- merge(final_file,emojis,by.x = "emoticon",by.y = "R.encoding",all.x = T)

#remove mentions and @s in the tweet because it adds no value to our analysis
final_file$text = str_replace_all(final_file$text,'@\\S+',"")

#Also removing RT because that information is already captured in the column isRetweet
final_file$text=str_replace_all(final_file$text,'rt ','')

#Removing links from the tweets 
final_file$text = gsub('http\\S+\\s*', "", final_file$text)

#Let's not forget to remove the R encoding since we have extracted the sentiment we needed from it
final_file$text = gsub("<.*>",'',final_file$text)

#remove all other non aplha numeric characters
final_file$text = gsub("[^0-9A-Za-z/// ]", "", final_file$text, T)


#there are some duplicate tweets from the exact same person at the same time. 
#and a lot of duplicate tweets are probably promotional tweets 
#The first step is to get out all the tweets that are not retweets and the remove duplicates
notRetweets = subset(final_file,isRetweet == F)
notRetweets = notRetweets[!duplicated(notRetweets["text"]),]
notRetweets = notRetweets[!duplicated(notRetweets["text"]),]

#I noticed something pretty weird with the retweeted tweets. I found duplicates of the same person retweeting the same thing within the same second
#Pretty dodgy eh? So I factored that in and removed the duplicated
Retweet = subset(final_file,isRetweet == T)
Retweets = Retweet[!duplicated(Retweet["text"]),]
Retweets = Retweet[!duplicated(Retweet["ScreenName"]),]

#Lets just put these two datasets together
final_file = rbind(Retweets,notRetweets)

final_file = rbind(Retweets,notRetweets)

#some more cleaning to be done and most of it is related to konga. i had to do these painstainkingly.
#you'd have to go through your text variable in your data and remove any observation
#that doesnt relate to our analysis

final_file = subset(final_file,!grepl("donkey konga",text))
final_file = subset(final_file,!grepl("konga shaker",text))
final_file = subset(final_file,!grepl("abadango",text))
final_file = subset(final_file,!grepl("kalu",text))
final_file = subset(final_file,!grepl("amazing dancer",text))
final_file = subset(final_file,!grepl("lorile",text))
final_file = subset(final_file,!grepl("characters based on",text))
final_file = subset(final_file,!grepl("so clean vs konga",text))
final_file = subset(final_file,!grepl("play konga",text))
final_file = subset(final_file,!grepl("konga zspawty",text))
final_file = subset(final_file,!grepl("to play konga",text))
final_file = subset(final_file,!grepl("konga drum",text))
final_file = subset(final_file,!grepl("konga and big d",text))
final_file = subset(final_file,!grepl("bobc2",text))

#remove those pesky NAs in the description column and convert them to empty strings
final_file$Description = apply(data.frame(final_file$Description),1,function(x) ifelse(is.na(x),"",x))

#Now we are going to concatenate the tweets and emoji description
final_file$text = paste(final_file$text,tolower(final_file$Description))

# I assume that because some description were empty we ould have some trailing spaces.
#Let's remove trailing and leading spaces one last time just incase.
final_file$text = gsub("^\\s+|\\s+$", "",final_file$text)

#From the tidytext package, we would use the AFINN lexicon which scores word between -5 to 5 based on
#negative or positive sentiment
lexicon = sentiments %>% filter(lexicon == "AFINN")

#This function uses the AFINN lexicon we loop through the tweets and score them accordingly 
#It also takes into account when the word 'not' is in a sentence. If the word 'not' is there,
#I definitely need to look at the next word and check if that not + word exists in the lexicon
#If it does not I check if the second word alone exists in the lexicon and simply 
#reverse the sign of the score e.g "not great". great exists with a score of 3 and with this function
#not great would have a score of -3. Pretty awesome eh?
sentiment_score = function(sentence) {
  score = 0
  words = str_split(sentence, " ")
  words = unlist(words)
  for (i in 1:length(words)) {
    if (words[i] == "not") {
      word = paste("not", words[i + 1])
      word.split = unlist(str_split(word, " "))
      if (word %in% lexicon$word == T) {
        score = score + lexicon$score[lexicon$word == word]
        
      }
      else if (word.split[2] %in% lexicon$word == T) {
        score = score - lexicon$score[lexicon$word == word.split[2]]
        
      }
      
      
    }
    else if (i > 1 && words[i - 1] == "not")
      next
    
    else if (words[i] %in% lexicon$word == T) {
      score = score + lexicon$score[lexicon$word == words[i]]
      
    }
    
    
  }
  return (score)
  
}


#Let's apply this function to every row in the data. So much faster than a for loop!
final_file$score=apply(data.frame(final_file$text),1,sentiment_score)

#A Histogram of the scores
hist(final_file$score[final_file$score !=0])

table(final_file$score[final_file$score !=0]>0)




#Let's create a term-frequency document using the tm package by creating a corpus of all the words in our data
corpus = Corpus(VectorSource(tweetDataFinal$text[tweetDatafinal$score !=0]))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, content_transformer(stripWhitespace))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeWords, c("etisalat","9ja","etisalat_9ja","vodacom","nigeria","home","sleep","start","tweet","phone","aaaaay","face","datamustfall","datamustfal","network","data","call","amp"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, removeWords, removePunctuation(stopwords("en")))


frequencies = DocumentTermMatrix(corpus)

frequencies.common = removeSparseTerms(frequencies,0.9996)

etisalatTweets = as.data.frame(as.matrix(frequencies.common))

termFreq = data.frame(word = colnames(etisalatTweets),frequency = colSums(etisalatTweets))

wordcloud(colnames(etisalatTweets), colSums(etisalatTweets), scale = c(4, 0.5),colors = 'darkgreen')




