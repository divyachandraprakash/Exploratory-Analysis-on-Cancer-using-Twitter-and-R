library("twitteR")
library("ROAuth")
library(wordcloud)
library(RColorBrewer)
library(tm)
library(plyr)
library(ggplot2)
library(sentiment)
library(data.table)
library(topicmodels)

#authentication
load("twitter authentication.Rdata")
registerTwitterOAuth(cred)


#data collection
m8 = searchTwitter("#prostatecancer", n=200, geocode="18.9750,72.8258,1000mi",lang="en",cainfo="cacert.pem")
c1 = searchTwitter("#prostatecancer", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c2 = searchTwitter("#lung cancer", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c3 = searchTwitter("#breastcancer", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c4 = searchTwitter("#oralcancer", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c5 = searchTwitter("#lymphoma", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c6 = searchTwitter("#cancer research", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c7 = searchTwitter("#cancer treatment", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
c8 = searchTwitter("#cancer patient", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
m9 = searchTwitter("#cancer treatment", n=200, geocode="18.9750,72.8258,1000mi",lang="en",cainfo="cacert.pem")
m10 = searchTwitter("#cancer patient", n=200, geocode="18.9750,72.8258,1000mi",lang="en",cainfo="cacert.pem")
m11 = searchTwitter("#cancer help", n=200, geocode="18.9750,72.8258,1000mi",lang="en",cainfo="cacert.pem")
c11 = searchTwitter("#cancer help", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
m3 = searchTwitter("#liver cancer", n=200, geocode="18.9750,72.8258,1000mi",lang="en",cainfo="cacert.pem")
c9 = searchTwitter("#liver cancer", n=200, geocode="13.0839,80.2700,1000mi",lang="en",cainfo="cacert.pem")
chemo1 = searchTwitter("#chemotherapy", n=1500,lang="en",cainfo="cacert.pem")

lon1= searchTwitter("#breastcancer",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon2= searchTwitter("#leukemia",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon3= searchTwitter("#lungcancer",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon4= searchTwitter("#lymphoma",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon5= searchTwitter("#tumor",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon6= searchTwitter("#cervicalcancer",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon7= searchTwitter("#prostatecancer",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon8= searchTwitter("#cancerresearch",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon9= searchTwitter("#bladder cancer",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon10= searchTwitter("#cancer treatment",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon11= searchTwitter("#cancer patient",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon12= searchTwitter("#cancer help",geocode="51.5072,0.1275,1000mi", n=1500, lang="en", cainfo="cacert.pem")
lon13= searchTwitter("#chemotherapy", n=1500, lang="en", cainfo="cacert.pem")

#dataset
india=c(c1,c11,c2,c3,c4,c5,c6,c7,c8,c9,m1,m10,m11,m2,m3,m4,m5,m6,m7,m8,m9,chemo1)
uk=c(lon1,lon2,lon3,lon4,lon5,lon6,lon7,lon8,lon9,lon10,lon11,lon12,lon13)

#data frame
india.df <- do.call("rbind", lapply(india, as.data.frame))
uk.df <- do.call("rbind", lapply(uk, as.data.frame)) 

#corpus and text cleaning
india.corpus <- Corpus(VectorSource(india.df$text))
india.corpus <- tm_map(india.corpus, content_transformer(tolower))
india.corpus <- tm_map(india.corpus, content_transformer(removePunctuation))
india.corpus <- tm_map(india.corpus, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
india.corpus <- tm_map(india.corpus, content_transformer(removeURL))
myStopwords <- c(stopwords("english"), "available","via","and","or","about","heres","amp","nci","ncjs","et","ncis","can","according","associated","butt","buynowgt","highly","know","one","polls","side","today","use","wearnig","back","lady","new","min","r","stats","whoops","every","take","tweet","glaring","miss","million","us","screening","must","min","varadhkrish")
india.corpus <- tm_map(india.corpus, removeWords, myStopwords)

uk.Corpus <- Corpus(VectorSource(uk.df$text)) 
uk.Corpus <- tm_map(uk.Corpus, content_transformer(tolower)) 
uk.Corpus <- tm_map(uk.Corpus, content_transformer(removePunctuation)) 
uk.Corpus <- tm_map(uk.Corpus, content_transformer(removeNumbers)) 
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
uk.Corpus <- tm_map(uk.Corpus, content_transformer(removeURL)) 
myStopwords <- c(stopwords("english"), "available","via","and","or","about","heres","amp","nci","ncjs","et","ncis","can","lady","tells","wef","w","wearing","turned","driving","tools","side","personalize") 
uk.Corpus <- tm_map(uk.Corpus, removeWords, myStopwords) 

#TDM
india.tdm <- TermDocumentMatrix(india.corpus, control=list(wordLengths=c(1,Inf)))
uk.tdm <- TermDocumentMatrix(uk.Corpus, control=list(wordLengths=c(1,Inf)))
#inspect(uk.tdm[,])

#frequency bar plot
findFreqTerms(india.tdm, lowfreq=15)
india.tf <- rowSums(as.matrix(india.tdm))
india.tf <- subset(india.tf, india.tf>=15)
barplot(india.tf, las=2)

findFreqTerms(uk.tdm, lowfreq=20) 
uk.tf <- rowSums(as.matrix(uk.tdm)) 
uk.tf <- subset(uk.tf, uk.tf>=20) 
barplot(uk.tf, las=2)

#association
findAssocs(india.tdm,"cervicalcancer", 0.25)
findAssocs(uk.tdm, "lungcancer", 0.25) 

#word cloud
wordFreq <- sort(rowSums(as.matrix(india.tdm)), decreasing=TRUE)
set.seed(375)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=5, random.order=F,colors=brewer.pal(8,"Dark2"),random.color=TRUE)

wordFreq <- sort(rowSums(as.matrix(uk.tdm)), decreasing=TRUE) 
#set.seed(375) 
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=2, random.order=F,colors=brewer.pal(8, "Dark2"),random.color= TRUE) 

#hierarchical clustering 
india.tdm2 <- removeSparseTerms(india.tdm, sparse=0.95)
india.m2 <- as.matrix(india.tdm2)
india.distMatrix <- dist(scale(india.m2))
fit <- hclust(india.distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=5)
(groups <- cutree(fit, k=5))

uk.tdm2 <- removeSparseTerms(uk.tdm, sparse=0.95)
uk.m2 <- as.matrix(uk.tdm2)
uk.distMatrix <- dist(scale(uk.m2))
fit <- hclust(uk.distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=5)
(groups <- cutree(fit, k=5))

#k-means clustering
india.m3 <- t(india.m2)
set.seed(122)
k <- 5
india.kmeansResult <- kmeans(india.m3, k)
round(india.kmeansResult$centers, digits=3)
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(india.kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
}

uk.m3 <- t(uk.m2)
set.seed(122)
k <- 5
uk.kmeansResult <- kmeans(uk.m3, k)
round(uk.kmeansResult$centers, digits=3)
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(uk.kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
}

#sentiment analysis
india.text = laply(india, function(t) t$getText() )
india.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", india.text)
 india.text = gsub("@\\w+", "", india.text)
india.text = gsub("[[:punct:]]", "", india.text)
india.text = gsub("[[:digit:]]", "", india.text)
india.text = gsub("http\\w+", "", india.text)
 india.text = gsub("[ \t]{2,}", "", india.text)
 india.text = gsub("^\\s+|\\s+$", "", india.text)
try.error = function(x)
{
# create missing value
y = NA
# tryCatch error
try_error = tryCatch(tolower(x), error=function(e) e)
# if not an error
if (!inherits(try_error, "error"))
y = tolower(x)
# result
return(y)
}
india.text = sapply(india.text, try.error)
india.text = india.text[!is.na(india.text)]
names(india.text) = NULL
class_emo = classify_emotion(india.text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(india.text, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=india.text, emotion=emotion, polarity=polarity,stringsAsFactors=FALSE)
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion))  + geom_bar(aes(y=..count.., fill=emotion))+scale_fill_brewer(palette="Dark2") + labs(x="emotion categories", y="number of tweets") +  ggtitle("Sentiment Analysis of Tweets about Cancer\n(classification by emotion)")
ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="number of tweets") + ggtitle("Sentiment Analysis of Tweets about Cancer\n(classification by polarity)")

uk.text = laply(uk, function(t) t$getText() )
uk.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", uk.text)
uk.text = gsub("@\\w+", "", uk.text)
uk.text = gsub("[[:punct:]]", "", uk.text)
uk.text = gsub("[[:digit:]]", "", uk.text)
uk.text = gsub("http\\w+", "", uk.text)
uk.text = gsub("[ \t]{2,}", "", uk.text)
uk.text = gsub("^\\s+|\\s+$", "", uk.text)
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
uk.text = sapply(uk.text, try.error)
uk.text = uk.text[!is.na(uk.text)]
names(uk.text) = NULL
class_emo = classify_emotion(uk.text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(uk.text, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=uk.text, emotion=emotion, polarity=polarity,stringsAsFactors=FALSE)
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion))  + geom_bar(aes(y=..count.., fill=emotion))+scale_fill_brewer(palette="Dark2") + labs(x="emotion categories", y="number of tweets") +  ggtitle("Sentiment Analysis of Tweets about Cancer\n(classification by emotion)")
ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="number of tweets") + ggtitle("Sentiment Analysis of Tweets about Cancer\n(classification by polarity)")


#topic modelling LDA
india.dtm <- as.DocumentTermMatrix(india.tdm)
india.lda <- LDA(india.dtm, k = 8)
term <- terms(india.lda, 4)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term
topic <- topics(india.lda, 1)
topics <- data.frame(date=as.IDate(india.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",fill=term[topic], position="stack")

uk.dtm <- as.DocumentTermMatrix(uk.tdm)
uk.lda <- LDA(uk.dtm, k = 8)
term <- terms(uk.lda, 4)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term
topic <- topics(uk.lda, 1)
topics <- data.frame(date=as.IDate(uk.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",fill=term[topic], position="stack")
