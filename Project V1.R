library(tidyr)
library(dplyr)
library(quanteda)
library(ggplot2)
library(ANLP)

create_cleantext <- function(text) {
clean_text <- tokens(text, what = c("word"), remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE,
                     remove_url = TRUE, remove_separators = TRUE, verbose = TRUE)
clean_text <- tokens_tolower(clean_text)
return(clean_text)
}

create_1gram <- function(clean_text){
unigram <- tokens_ngrams(clean_text, n = 1, concatenator = " ")
dfm1 <- dfm(unigram)

gram11 <- data.frame(word = featnames(dfm1), freq = colSums(dfm1),
                     row.names = NULL, stringsAsFactors = FALSE)

gram11 <- gram11 %>% arrange(desc(freq))
return(gram11)
}

create_2gram <- function(clean_text){
  bigram <- tokens_ngrams(clean_text, n = 2, concatenator = " ")
  dfm2 <- dfm(bigram)
  gram12 <- data.frame(word = featnames(dfm2), freq = colSums(dfm2),
                       row.names = NULL, stringsAsFactors = FALSE)
  gram12 <- gram12 %>% arrange(desc(freq))
  return(gram12)
}

create_3gram <- function(clean_text){
  trigram <- tokens_ngrams(clean_text, n = 3, concatenator = " ")
  dfm3 <- dfm(trigram)
  gram13 <- data.frame(word = featnames(dfm3), freq = colSums(dfm3),
                       row.names = NULL, stringsAsFactors = FALSE)
  gram13 <- gram13 %>% arrange(desc(freq))
  
  return(gram13)
}

create_4gram <- function(clean_text){
  fourgram <- tokens_ngrams(clean_text, n = 4, concatenator = " ")
  dfm4 <- dfm(fourgram)
  gram14 <- data.frame(word = featnames(dfm4), freq = colSums(dfm4),
                       row.names = NULL, stringsAsFactors = FALSE)
  gram14 <- gram14 %>% arrange(desc(freq))
  
  return(gram14)
}

setwd("D:/Coursera/Project/final/en_US")
en_twitter <- readTextFile("en_US.twitter.txt",encoding = 'UTF-8')
twitter_clean <- create_cleantext(en_twitter)
rm(en_twitter)
gc()

twt_gram1 <- create_1gram(twitter_clean)
twt_gram11 <- head(twt_gram1, n = round(nrow(twt_gram1) * 0.2))
rm(twt_gram1)
write.csv(twt_gram11,"D:/Coursera/Project/tweet_gram1.csv",row.names = F)
# rm(twt_gram11)
gc()

twt_gram2 <- create_2gram(twitter_clean)
twt_gram12 <- head(twt_gram2, n = round(nrow(twt_gram2) * 0.2))
rm(twt_gram2)
write.csv(twt_gram12,"D:/Coursera/Project/tweet_gram2.csv",row.names = F)
# rm(twt_gram12)
gc()

twt_gram3 <- create_3gram(twitter_clean)
twt_gram13 <- head(twt_gram3, n = round(nrow(twt_gram3) * 0.2))
rm(twt_gram3)
write.csv(twt_gram13,"D:/Coursera/Project/tweet_gram3.csv",row.names = F)
# rm(twt_gram13)
gc()

twt_gram4 <- create_4gram(twitter_clean)
twt_gram14 <- head(twt_gram4, n = round(nrow(twt_gram4) * 0.2))
rm(twt_gram4)
write.csv(twt_gram14,"D:/Coursera/Project/tweet_gram4.csv",row.names = F)
# rm(twt_gram14)
rm(twitter_clean)
gc()

en_blogs <- readTextFile("en_US.blogs.txt",encoding = 'UTF-8')
blogs_clean <- create_cleantext(en_blogs)
rm(en_blogs)

blg_gram1 <- create_1gram(blogs_clean)
blg_gram1_a <- blg_gram1[!(blg_gram1$word %in% twt_gram11$word), ]
blg_gram11 <- head(blg_gram1_a, n = round(nrow(blg_gram1) * 0.2))
twt_gram11 <- rbind(twt_gram11,blg_gram11)
rm(blg_gram1, blg_gram11,blg_gram1_a)
gc()
write.csv(twt_gram11,"D:/Coursera/Project/twt_gram11.csv",row.names = F)

blg_gram2 <- create_2gram(blogs_clean)
blg_gram2_a <- blg_gram2[!(blg_gram2$word %in% twt_gram12$word), ]
blg_gram12 <- head(blg_gram2_a, n = round(nrow(blg_gram2) * 0.2))
twt_gram12 <- rbind(twt_gram12,blg_gram12)
rm(blg_gram2, blg_gram12,blg_gram2_a)
gc()
write.csv(twt_gram12,"D:/Coursera/Project/twt_gram12.csv",row.names = F)

blg_gram3 <- create_3gram(blogs_clean)
blg_gram3_a <- blg_gram3[!(blg_gram3$word %in% twt_gram13$word), ]
blg_gram13 <- head(blg_gram3_a, n = round(nrow(blg_gram3) * 0.2))
twt_gram13 <- rbind(twt_gram13,blg_gram13)
rm(blg_gram3, blg_gram13,blg_gram3_a)
gc()
write.csv(twt_gram13,"D:/Coursera/Project/twt_gram13.csv",row.names = F)

blg_gram4 <- create_4gram(blogs_clean)
blg_gram4_a <- blg_gram4[!(blg_gram4$word %in% twt_gram14$word), ]
blg_gram14 <- head(blg_gram4_a, n = round(nrow(blg_gram4) * 0.2))
twt_gram14 <- rbind(twt_gram14,blg_gram14)
rm(blg_gram4, blg_gram14,blg_gram4_a)
gc()
write.csv(twt_gram14,"D:/Coursera/Project/twt_gram14.csv",row.names = F)

rm(twt_gram11, twt_gram12, twt_gram13, twt_gram14)
rm(blogs_clean)
gc()
# en_twitter <- sample(en_twitter, length(en_twitter) * 0.05, replace = FALSE)
# en_blogs <- sample(en_blogs, length(en_blogs) * 0.05, replace = FALSE)
# en_news <- sample(en_news, length(en_news) * 0.05, replace = FALSE)

en_news <- readTextFile("en_US.news.txt",encoding = 'UTF-8')
news_clean <- create_cleantext(en_news)
rm(en_news)

nws_gram1 <- create_1gram(news_clean)
twt_gram11 <- read.csv("D:/Coursera/Project/twt_gram11.csv",header = TRUE)
nws_gram1_a <- nws_gram1[!(nws_gram1$word %in% twt_gram11$word), ]
nws_gram11 <- head(nws_gram1_a, n = round(nrow(nws_gram1) * 0.2))
twt_gram11 <- rbind(twt_gram11,nws_gram11)
write.csv(twt_gram11,"D:/Coursera/Project/gram1.csv",row.names = F)
rm(nws_gram1, nws_gram11,nws_gram1_a, twt_gram11)
gc()

nws_gram2 <- create_2gram(news_clean)
twt_gram12 <- read.csv("D:/Coursera/Project/twt_gram12.csv",header = TRUE)
nws_gram2_a <- nws_gram2[!(nws_gram2$word %in% twt_gram12$word), ]
nws_gram12 <- head(nws_gram2_a, n = round(nrow(nws_gram2) * 0.2))
twt_gram12 <- rbind(twt_gram12,nws_gram12)
write.csv(twt_gram12,"D:/Coursera/Project/gram2.csv",row.names = F)
rm(nws_gram2, nws_gram12,nws_gram2_a, twt_gram12)
gc()


nws_gram3 <- create_3gram(news_clean)
twt_gram13 <- read.csv("D:/Coursera/Project/twt_gram13.csv",header = TRUE)
nws_gram3_a <- nws_gram3[!(nws_gram3$word %in% twt_gram13$word), ]
nws_gram13 <- head(nws_gram3_a, n = round(nrow(nws_gram3) * 0.2))
twt_gram13 <- rbind(twt_gram13,nws_gram13)
write.csv(twt_gram13,"D:/Coursera/Project/gram3.csv",row.names = F)
rm(nws_gram3, nws_gram13,nws_gram3_a,twt_gram13)
gc()

nws_gram4 <- create_4gram(news_clean)
rm(news_clean)
twt_gram14 <- read.csv("D:/Coursera/Project/twt_gram14.csv",header = TRUE)
nws_gram4_a <- nws_gram4[!(nws_gram4$word %in% twt_gram14$word), ]
nws_gram14 <- head(nws_gram4_a, n = round(nrow(nws_gram4) * 0.2))
twt_gram14 <- rbind(twt_gram14,nws_gram14)
write.csv(twt_gram14,"D:/Coursera/Project/gram4.csv",row.names = F)
rm(nws_gram4, nws_gram14,nws_gram4_a,twt_gram14)
gc()

rm(en_twitter,en_blogs,en_news)
gc()



blg_gram1 <- create_1gram(blogs_clean)
blg_gram2 <- create_2gram(blogs_clean)
blg_gram3 <- create_3gram(blogs_clean)
blg_gram4 <- create_4gram(blogs_clean)

nws_gram1 <- create_1gram(news_clean)
nws_gram2 <- create_2gram(news_clean)
nws_gram3 <- create_3gram(news_clean)
nws_gram4 <- create_4gram(news_clean)

rm(twitter_clean,blogs_clean,news_clean)
gc()

gram1 <- read.csv("D:/Coursera/Project/gram1.csv",header = TRUE)
gram1 <- gram1 %>% arrange(desc(freq))
gram1 <- head(gram1, n = round(nrow(gram1) * 0.25))

gram2 <- read.csv("D:/Coursera/Project/gram2.csv",header = TRUE)
gram2 <- gram2 %>% arrange(desc(freq))
gram2 <- head(gram2, n = round(nrow(gram2) * 0.25))

gram3 <- read.csv("D:/Coursera/Project/gram3.csv",header = TRUE)
gram3 <- gram3 %>% arrange(desc(freq))
gram3 <- head(gram3, n = round(nrow(gram3) * 0.25))

gram4 <- read.csv("D:/Coursera/Project/gram4.csv",header = TRUE)
gram4 <- gram4 %>% arrange(desc(freq))
gram4 <- head(gram4, n = round(nrow(gram4) * 0.25))

allgrams <- list(gram4, gram3, gram2, gram1)
blogsgrams <- list(blg_gram4, blg_gram3, blg_gram2, blg_gram1)
newsgrams <- list(nws_gram4, nws_gram3, nws_gram2, nws_gram1)


write.csv(gram4,"D:/Coursera/Project/ngrams/gram4.csv",row.names = F)
write.csv(gram3,"D:/Coursera/Project/ngrams/gram3.csv",row.names = F)
write.csv(gram2,"D:/Coursera/Project/ngrams/gram2.csv",row.names = F)
write.csv(gram1,"D:/Coursera/Project/ngrams/gram1.csv",row.names = F)

gram2 <- read.csv("D:/Coursera/Project/ngrams/gram2.csv",header = TRUE)
gram2$word <- as.character(gram2$word)
create_pred <- function(x){
  paste(word(x, -1), collapse=' ')
}

create_2input <- function(x){
  paste(word(x, 1), collapse=' ')
}
gram2$word_input <- sapply(gram2$word,create_2input)
gram2$word_pred <- sapply(gram2$word,create_pred)
gram2 <- head(gram2, n = round(nrow(gram2) * 0.5))
gram2$word <- NULL
write.csv(gram2,"D:/Coursera/Project/ngrams/gram2.csv",row.names = F)


gram3 <- read.csv("D:/Coursera/Project/ngrams/gram3.csv",header = TRUE)
gram3$word <- as.character(gram3$word)
create_3input <- function(x){
  paste(word(x, (1:2)), collapse=' ')
}
gram3$word_input <- sapply(gram3$word,create_3input)
gram3$word_pred <- sapply(gram3$word,create_pred)
gram3 <- head(gram3, n = round(nrow(gram3) * 0.5))
gram3$word <- NULL
write.csv(gram3,"D:/Coursera/Project/ngrams/gram3.csv",row.names = F)

gram4 <- read.csv("D:/Coursera/Project/ngrams/gram4.csv",header = TRUE)
gram4$word <- as.character(gram4$word)
create_4input <- function(x){
  paste(word(x, (1:3)), collapse=' ')
}
gram4$word_input <- sapply(gram4$word,create_4input)
gram4$word_pred <- sapply(gram4$word,create_pred)
gram4$word <- NULL
gram4 <- head(gram4, n = round(nrow(gram4) * 0.5))
write.csv(gram4,"D:/Coursera/Project/ngrams/gram4.csv",row.names = F)
