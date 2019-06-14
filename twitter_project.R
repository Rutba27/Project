#**********************************Twitter sentiment Analysis ********************************
#********************* on SunRisers Hyderabad's team performance in ipl 2018 *****************

#loading the library

library(caTools)
library(dplyr)
library(tm)
library(RTextTools)
library(Matrix)
library(ggplot2)
library(purrr)


#*********** loading the tweets (as csv) ************
getwd()

setwd("D:/Text Mining/Project")

tweet_data <- read.csv("D:/Text Mining/Project/twitter_scrap_@SunRisers.csv",header = TRUE,
                       stringsAsFactors = FALSE,encoding = "UTF-8")
summary(tweet_data)



#*********** visualizing the tweets *****************

new_data <- tweet_data

#taking the date without Hrs and Min

str(new_data)
head (new_data$created)

new_data$created1 <- as.Date(new_data$created,'%d-%m-%Y %H:%M')

head(new_data$created1)


# user vs time of the tweet

ggplot(new_data,aes(x=created1,y=screenName),color="lightpink") + 
  geom_point() + ylab("Twitter username") + xlab("Time")


#No of tweet count

#install.packages("chron")
library(chron)


ggplot(new_data, aes(x=new_data$created1)) +
geom_bar(stat="count",fill="purple")+
  xlab("Date") + ylab("Count") + 
  labs(title="Tweets per Day")


# Table of the number of tweets per user
d <- as.data.frame(table(new_data$screenName))
d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
names(d) <- c("User","Tweets")
head(d)

# Plot the table for the top 20 tweeters
barplot(head(d$Tweets, 20), names=head(d$User, 20), horiz=T, las=1, 
        main="Top 20: Tweets per User", col=rainbow(20),cex.names = .7)

# select top retweeted tweets
table(new_data$retweetCount)
selected <- which(new_data$retweetCount >= 1000)

#subsetting the dataframe for plotting
toptw <- subset(new_data,new_data$retweetCount>1000)

# plot them
dates <- strptime(toptw$created, format="%d-%m-%Y")
plot(x=dates, y=toptw$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], toptw$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], toptw$retweetCount[selected],
     toptw$text[selected], col=colors, cex=.5)


#tweet that has been retweeted the most

maxtw <- max(new_data$retweetCount) 
maxs <- subset(new_data[,c(2,6,12,13)],new_data$retweetCount==maxtw)
maxs[1,]


#******* Data Preprocessing ***********

#**Cleaning the tweet text 


#creating corpus of the text 
myCorpus1 = VCorpus(VectorSource(tweet_data$text))

lapply(myCorpus1[1:5],as.character)


#removing retweets (RT)

removeRT = function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",x)
myCorpus1 = tm_map(myCorpus1,content_transformer(removeRT))

lapply(myCorpus1[1:5],as.character)

removegr=function(x) gsub("<[^[:space:]]*","",x)
myCorpus1=tm_map(myCorpus1,content_transformer(removegr))

lapply(myCorpus1[1:5],as.character)

#removing URLs
removeURL=function(x) gsub("http[^[:space:]]*", "", x)
myCorpus1=tm_map(myCorpus1, content_transformer(removeURL))

lapply(myCorpus1[1:5],as.character)


#removing words starting with "@"
removeName=function(x) gsub("@[a-z A-Z 0-9]\\w+","", x)
myCorpus1=tm_map(myCorpus1, content_transformer(removeName))

lapply(myCorpus1[1:5],as.character)

#remove punctuation
removeNumPunct=function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus1=tm_map(myCorpus1, content_transformer(removeNumPunct))

lapply(myCorpus1[1:5],as.character)

#text converted to lowercase
myCorpus1 = tm_map(myCorpus1, content_transformer(tolower))

lapply(myCorpus1[1:5],as.character)

#remove stopwords
myCorpus1= tm_map(myCorpus1, removeWords, stopwords("english"))

lapply(myCorpus1[1:5],as.character)


#manually adding stopwords
myStopwords <- c("can", "say","one","way","use","also","howev","tell","will",
                 "much","need","take","tend","even","like","particular","rather","said",
                 "get","well","make","ask","come","end","first","two","help","often","may",
                 "might","see","someth","thing","point","post","look","right","now","think","'ve ",
                 "'re ","delig","ndtv","and","the","rajiv","gandhi","stadium","vs","here")

#remove custom stopwords
myCorpus1 = tm_map(myCorpus1,removeWords,myStopwords)

#Strip spaces
myCorpus1= tm_map(myCorpus1, stripWhitespace)
lapply(myCorpus1[1:5],as.character)

#stem the document
myCorpus1= tm_map(myCorpus1, stemDocument, language = "english")
lapply(myCorpus1[1:5],as.character)


#create a wordcloud
library(RColorBrewer)
col=brewer.pal(6,"Dark2")

library(wordcloud)
wordcloud(myCorpus1, min.freq=25,rot.per = 0.25,scale = c(2,1),
          random.color=T,random.order=F,colors=col)


#transform to plain text
mydoc1 = tm_map(myCorpus1, PlainTextDocument)
mydoc1[[1]]$content

#save the cleaned file 
cleandf <- data.frame(text = unlist(sapply(mydoc1,'[',"content")),stringsAsFactors = FALSE)

write.csv(cleandf, file = "Twitter_file.csv")


#Create term document matrix

library(slam)

dtm1 <- TermDocumentMatrix(myCorpus1)

# Find the total occurances of each word in all docs
rowTotals <-  row_sums(dtm1)

# As the Document Term matrix is very large we would be creating a sub matrix by
#selecting words that occur > 20 times in all docs together to know which are the terms used extensively
dtm2 <- dtm1[,which(rowTotals > 20)]
dtm2

#creating dtm as matrix and sorting it

m1 <- as.matrix(dtm2)

m1 <- sort(rowSums(m1),decreasing=TRUE)

#converting to dataframe

m2 <- data.frame(word = names(m1),freq=m1)

head(m2)

barplot(m2[1:15,]$freq, las = 2, names.arg = m2[1:15,]$word,col = "cyan",
        main = "Most frequent words",ylab = "Word frequency")
        

#****** Sentiment Analysis using lexicon method **************


#Reading the positive and negetive word files 

pos.words <- scan(file="D:/Text Mining/Assignment/positive_words.txt",what = "character",
                  comment.char = ";")
neg.words <- scan(file="D:/Text Mining/Assignment/negetive_words.txt",what = "character",
                  comment.char = ";")

#sentiment function

score.sentiment = function(sentences, pos.words, neg.words, .progress="none")
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
    {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub("\\d+", "", sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, "\\s+")
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches)-sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# retrieve the cleaned twitter text
data1 <- cleandf

data1$text <- as.factor(data1$text)

#Apply sentiment function to the cleaned text and get scores

text_scores <- score.sentiment(data1$text,pos.words,neg.words)

summary(text_scores$score)

#histogram showing the range of scores

ggplot(text_scores, aes(score)) + 
  geom_histogram(color="blue",fill="lightblue",bins = 30) +
  labs(title="Score of tweets")

#summarizing the score into positive , negetive and nuetral to get polarity
#score < 0 are taken as negetive, score = 0 is nuetral and score > 0 are positive

tweet_data$date <- as.Date(tweet_data$created,'%d-%m-%Y %H:%M')

stat<- text_scores
stat$created <- tweet_data$date
stat$created <- as.Date(stat$created)

stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())

#wordcloud of positive sentiment score

pos.sen <- stat[stat$score>0,c(2)]
posC <- Corpus(VectorSource(postive.sen))
wordcloud(posC,min.freq = 10,max.words = 50,random.color = FALSE,
          random.order = FALSE,colors = brewer.pal(6,"PuOr"))

#wordcloud of negetive sentiment score
neg.sen <- stat[stat$score< 0,c(2)]
negC <- Corpus(VectorSource(neg.sen))
wordcloud(negC,min.freq = 10,max.words = 25,random.color = FALSE,
          random.order = FALSE,colors = brewer.pal(6,"Reds"))

#tweet/tweets with the max positive score
 psc <- max(stat$score)
 posstr <- subset(stat[,c(1,2)],stat$score==psc)
 posstr
 
 #tweet/tweets with the max negetive score
 nsc <- min(stat$score)
 negstr <- subset(stat[,c(1,2)],stat$score==nsc)
 negstr

#write the stats into a file
write.csv(stat,file = "Twitter_score.csv")

#removing incomplete cases
stat1 <- stat[complete.cases(stat),]

#barplot showing Tweet polarity
ggplot(stat1,aes(tweet))+
  geom_bar(stat = "count",fill=brewer.pal(3,"YlGn"))+
  labs(title="Polarity of tweets",x="Tweet type")

#plotting the tweets along the timelines

ggplot(by.tweet, aes(created,number)) + 
  geom_line(aes(group=tweet,color = tweet),size=1) +
  geom_point(aes(group=tweet, color=tweet), size=2) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) 


#********************* Sentiment Analysis using ML techniques **************
library(text2vec)
library(caret)
library(glmnet)
library(purrrlyr)

#** Doc2Vec algorithm is used to analyse the tweet text which creates a numerical 
#representation of documents
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")


#reading the twitter score file

Adata <- read.csv(file="Twitter_score.csv",header = TRUE,
                  stringsAsFactors = FALSE)


Adata$tweet[which(Adata$tweet=="positive")]=1
Adata$tweet[which(Adata$tweet=="negative")]=0
Adata$tweet[which(Adata$tweet=="neutral")]=0

Adata$tweet <- as.numeric(Adata$tweet)


#split into train and test data

set.seed(111)
trainIndex <- createDataPartition(Adata$tweet, p = 0.7,list = FALSE, times = 1)

tweets_train <- Adata[trainIndex, ]
tweets_test <- Adata[-trainIndex, ]

#*** Vectorization 

#define preprocessing function and tokenization function

prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$X,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$X,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix of training data
vocab <- create_vocabulary(it_train)

vectorizer <- vocab_vectorizer(vocab)

dtm_train <- create_dtm(it_train, vectorizer)

# define tf-idf model
tfidf <- TfIdf$new()

# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)

# apply pre-trained tf-idf transformation to test data
#dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>% 
#  transform(tfidf)

#train the model using binomial regression

glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['tweet']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc")


#plot the ROC curve and find Auc of training data
plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

#predicted value of test data and its auc
preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
auc(as.numeric(tweets_test$sentiment), preds) 

#As seen above both the train and test data have a high auc (>.95) and 
#hence this model can be considered for knowing the positive sentiment of real time data


#******** testing the model with real time twitter data ****************
library(twitteR)
library(ggrepel)


# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

#*** fetching tweets 
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")

consumer_key="7rxTTh7U3sQ3VQsYvogPIXtIA"
consumer_secret="INa0yRU6Trxk9ydej6ichvKiFkDZJXhNqF7JY8CYHP2uIJfWlg"
access_token="977401767913693184-4edt6bRxPMEQBz42gg0UAPybL2o7sES"
access_secret="pqSSlYKzUmtdtClwZsyCrWyM6c9ywr9exr4P1MQQ9bDXq"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

result <- searchTwitter('@SunRisers', n = 2000, lang = 'en')
df_tweets <- do.call("rbind", lapply(result, as.data.frame))
df_tweets$text <- sapply(df_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
#** converting some symbols
  dmap_at('text', conv_fun)
  
#*** preprocessing and tokenization
  it_tweets <- itoken(df_tweets$text,
                      preprocessor = prep_fun,
                      tokenizer = tok_fun,
                      ids = df_tweets$id,
                      progressbar = TRUE)
  
#** creating vocabulary and document-term matrix
    dtm_tweets <- create_dtm(it_tweets, vectorizer)
  
#** transforming data with tf-idf
    dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)
  
#** predict probabilities of positiveness
    preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]
  
#** adding rates to initial dataset
    df_tweets$sentiment <- preds_tweets
  
  
#** color palette
    cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")
  
  set.seed(111)
  samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.05) # 5% for labeling
  
#** plotting the predicted results
  ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
    theme_minimal() +
    scale_color_gradientn(colors = cols, limits = c(0, 1),
                          breaks = seq(0, 1, by = 1/4),
                          labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                          guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
    geom_point(aes(color = sentiment), alpha = 0.8) +
    geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
    geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
    geom_smooth(size = 1.2, alpha = 0.2) +
    geom_label_repel(data = df_tweets[samp_ind, ],
                     aes(label = round(sentiment, 2)),
                     fontface = 'bold',
                     size = 2.5,
                     max.iter = 100) +
    theme(legend.position = 'bottom',
          legend.direction = "horizontal",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 15, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 10, face = "bold", color = 'black'),
          axis.text.x = element_text(size = 10, face = "bold", color = 'black')) +
    ggtitle("Tweets Sentiment rate (probability of positiveness)")
  
  
  
  
