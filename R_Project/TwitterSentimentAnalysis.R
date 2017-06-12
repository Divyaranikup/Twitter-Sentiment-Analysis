# TWITTER SENTIMENT ANALYSIS - on tweets about Donald Trump and Hillary Clinton

# load the required packages and libraries
if(!require(twitteR)) install.packages("twitteR",dependencies = T)
if(!require(plyr)) install.packages("plyr")
if(!require(tm)) install.packages("tm")
if(!require(stringr)) install.packages("stringr")
if(!require(Rstem)) install.packages("Rstem")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(wordcloud)) install.packages("wordcloud")
if(!require(sentiment)) install.packages("sentiment_0.2.tar.gz", repos = NULL, type="source")

library(twitteR)
library(plyr)
library(tm)
library(stringr)
library(Rstem)
library(ggplot2)  
library(RColorBrewer)
library(wordcloud)
library(sentiment)

consumer_key <- "ifM0CquZBPvnoixnfy1fDtUuI" # "consumer key"
consumer_secret<- "jfFVCYZjNIPSwIiIyi7GB1bt9RqfBSuiq1xUc34LpXGzr9Q5ap"# "consumer secret"
access_token <- "798234570420289536-SqdlcrBAp6Uw5BXDBeNlWC7EGKgq2tn" # "access token"
access_secret <- "dygj5jIggUCGZZ0S15dMiMXdS1zdiKTwUVurySKz5RmK3" # "access secret
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Fetch tweets from Twitter
trump_tweets <- searchTwitter('trump', n = 1500, lang = "en", resultType = "recent")
hillary_tweets <- searchTwitter('hillary', n = 1500, lang = "en", resultType = "recent")

#Extract text from tweets
trump_text <- sapply(trump_tweets, function(x) x$getText())
trump_text <- iconv(trump_text, 'UTF-8', 'ASCII')
hillary_text <- sapply(hillary_tweets, function(x) x$getText())
hillary_text <- iconv(hillary_text, 'UTF-8', 'ASCII')

# function to remove unwanted characters
clean.text = function(x)
{
  x = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  x = gsub("@\\w+", "", x)
  x = gsub("[[:punct:]]", "", x)
  x = gsub("[[:digit:]]", "", x)
  x = gsub("http\\w+", "", x)
  x = gsub("[ \t]{2,}", "", x)
  x = gsub("^\\s+|\\s+$", "", x)
  x = tolower(x)
  x = removeWords(x, c(stopwords("english"), "trump", "hillary", "donald", "clinton"))
  return(x)
}

# clean texts
trump_text = clean.text(trump_text)
hillary_text = clean.text(hillary_text)

# scan Positive and Negative words
pos = scan('positive-words.txt', what='character', comment.char = ';')
neg = scan('negative-words.txt', what='character', comment.char = ';')
source('sentiment.R')

# Evaluate sentiment score
analysis_trump = score.sentiment(trump_text, pos, neg)
table(analysis_trump$score)
analysis_hillary = score.sentiment(hillary_text, pos, neg)
table(analysis_hillary$score)

# Plot Histograms
hist(analysis_trump$score, main="Histogram for Trump's Sentiment Analysis", xlab="SentimentScore")
hist(analysis_hillary$score, main="Histogram for Hillary's Sentiment Analysis", xlab="SentimentScore")

# Plot comparison wordcloud between Trump and Hillary
trump_words = paste(trump_text, collapse=" ")
hillary_words = paste(hillary_text, collapse=" ")
all = c(trump_words, hillary_words)
# create corpus
corpus = Corpus(VectorSource(all))
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert as matrix
tdm = as.matrix(tdm)
# add column names
colnames(tdm) = c("Trump", "Hillary")
# comparison cloud
comparison.cloud(tdm, random.order=FALSE, colors = c("red", "#00B2FF"), title.size=1.5, max.words=500)

# classify emotion and get emotion best fit
class_emo = classify_emotion(trump_text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity and get polarity best fit
class_pol = classify_polarity(trump_text, algorithm="bayes")
polarity = class_pol[,4]

sent_df = data.frame(text=trump_text, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution by emotions and polarity
ggplot(sent_df, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title = "Sentiment Analysis of Tweets about Trump\n(classification by emotion)")

ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets", title = "Sentiment Analysis of Tweets about Trump\n(classification by polarity)")

# plot comparison word cloud between different types of emotions
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = trump_text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5)