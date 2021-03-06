library(wordcloud)
library(ngram)
library(caret)
library(stringi)
library(tm)
library(dbplyr)
library(tibble)
library(dplyr)
library(tidytext)

# Set seed and working directory
set.seed(12345)
setwd("M:\\r_projects\\final_project_app")

readTextFile <- function(fileName, warn=FALSE, skipNul=TRUE) {
  textFile <- file(fileName, open="r")
  textVector <- readLines(textFile, warn=warn, encoding="UTF-8", skipNul=skipNul)
  close(textFile)
  return(textVector)
}

## Download the Coursera-Swiftkey.zip file from the Coursera website
## and unzip the folder Coursera-Swiftkey to the working directory

## Read blogs 
blogsFileName <- ".\\Coursera-Swiftkey\\final\\en_US\\en_US.blogs.txt"
blogs <- readTextFile(blogsFileName)

## Read news
newsFileName <- ".\\Coursera-Swiftkey\\final\\en_US\\en_US.news.txt"
news <- readTextFile(newsFileName)

## Read twitters
twitterFileName <- ".\\Coursera-Swiftkey\\final\\en_US\\en_US.twitter.txt"
twitter <- readTextFile(twitterFileName)

## Count lines in sources
nBlogLines <- length(blogs)
nNewsLines <- length(news)
nTwitterLines <- length(twitter)
nTotalLines <- nBlogLines + nNewsLines + nTwitterLines

# Count number of words
blogsWords <- sum(stri_count_words(blogs))
newsWords <- sum(stri_count_words(news))
twitterWords <- sum(stri_count_words(twitter))
totalWords <- blogsWords + newsWords + twitterWords

## Summarize sources
sourceLines <- c(nBlogLines, nNewsLines, nTwitterLines, nTotalLines)
sourceWords <- c(blogsWords, newsWords, twitterWords, totalWords)
sourceWordsPerLine <- c(blogsWords/nBlogLines,
                        newsWords/nNewsLines,
                        twitterWords/nTwitterLines,
                        totalWords/nTotalLines
)
sourceSummary <- data.frame(sourceLines, sourceWords, sourceWordsPerLine)
rownames(sourceSummary) <- c("Blogs", "News", "Twitter", "Total")
colnames(sourceSummary) <- c("Number_of_Lines", "Number_of_Words", "Words_per_Line")
sourceSummary


#build function for text cleaning
textcleaning <- function(x){ 
  textclean <- x %>% 
    removeNumbers() %>%
    removeWords(stopwords("english")) %>%
    removePunctuation(preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE) %>%
    stripWhitespace()
  return(textclean)
}

tc <- textcleaning(twitter)
bc <- textcleaning(blogs)
nc <- textcleaning(news)

t3 <- sample(tc,10000)
b3 <- sample(bc,10000)
n3 <- sample(nc,10000)

#join all three tibbles in one object
# tbn <- bind_rows("twitter" = t3, "blogs" = b3, "news" = n3)
tbn <- tibble(sentences=c(t3,b3,n3))
tbn2 <- tbn %>% unnest_tokens(ngrams, sentences, token = "ngrams", n = 2)
tbn3 <- tbn %>% unnest_tokens(ngrams, sentences, token = "ngrams", n = 3)

totaltf <- function (x){
  ntf <- x %>% count(ngrams, sort=T)
  # total <- ntf %>% summarize(total = sum(n))
  # ntf <- left_join(ntf, total)
  # ntf <- mutate(ntf, tf = n/total)
  return(ntf)
}

TBN2 <- totaltf(tbn2)
TBN3 <- totaltf(tbn3)

bigrams <- strsplit(TBN2$ngrams, split = " ")
word1 <- sapply(bigrams,"[[",1)
word2 <- sapply(bigrams,"[[",2)

bigram_model <- data.frame(word1 = word1,word2 = word2,freq = TBN2$n,stringsAsFactors=FALSE)

saveRDS(bigram_model,"bigram.RData")
write.csv(bigram_model[bigram_model$freq > 1,],"bigram.csv",row.names=F)

trigrams <- strsplit(TBN3$ngrams, split = " ")
word1 <- sapply(trigrams,"[[",1)
word2 <- sapply(trigrams,"[[",2)
word3 <- sapply(trigrams,"[[",3)

trigram_model <- data.frame(word1 = word1,word2 = word2, word3 = word3, freq = TBN3$n,stringsAsFactors=FALSE)

saveRDS(trigram_model,"trigram.RData")
write.csv(trigram_model[trigram_model$freq > 1,],"trigram.csv",row.names=F)