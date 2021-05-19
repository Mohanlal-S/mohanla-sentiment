# mohanla-sentiment
library(syuzhet)
library(ggplot2)
library(corpus)
library(SnowballC)
library(wordcloud)
library(tm)
library(corpus)

text<-readLines(file.choose())
textdoc<-Corpus(VectorSource(text))
spacere<-content_transformer(function(x,pattern)gsub(pattern," ",x))

textdoc<-tm_map(textdoc,content_transformer(tolower))
textdoc<-tm_map(textdoc,content_transformer(removeNumbers))
textdoc<-tm_map(textdoc,removePunctuation)
textdoc<-tm_map(textdoc,stripWhitespace)
textdoc <- tm_map(textdoc, removeWords, stopwords("english"))
textdoc<-tm_map(textdoc,stemDocument)
textdoc<-tm_map(textdoc,removeWords,c("at","the","it's"))
textdoc<-tm_map(textdoc,removeWords,c("EUR","EUR1","A-S","(1)","(2)","US","mm","mln","mn","USD","(EURO)","MW","($44 million)","A-B-C","euro","m","US$"))



textdoc_dtm<-TermDocumentMatrix(textdoc)
dtm_m<-as.matrix(textdoc_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_v
dtm_d<-data.frame(word=names(dtm_v),freq=dtm_v)
View(dtm_d$word)
head(dtm_d,10)
barplot(dtm_d[1:10,]$freq,names.arg = dtm_d[1:10,]$word,col="red",main = "top 10",ylab = "word freq")
set.seed(123)
wordcloud(words=dtm_d$word,freq=dtm_d$freq,min.freq=5,max.words=100,random.error=FALSE,rot.per=0.40,color=brewer.pal(8,"Dark2"),shape="circle")
findAssocs(textdoc_dtm,terms=c("said","eur","year"),corlimit = 0.10)
findAssocs(textdoc_dtm,terms=findFreqTerms(textdoc_dtm,lowfreq=50),corlimit=0.15)
syuzhet_vector<-get_sentiment(textdoc,method="syuzhet")
data.frame(sign(syuzhet_vector))->syuzhet_vector
View(syuzhet_vector)
bing_vector<-get_sentiment(textdoc_dtm,method="bing")
data.frame(bing_vector)->b1
View(b1)
afinn_vector<-get_sentiment(textdoc,method="afinn")
data.frame(afinn_vector)->b1
View(sign(b1))
