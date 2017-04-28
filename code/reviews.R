# TextFeatures.R
# 1) Creates one-hot encoding of text of reviews, via Document-Term-Matrix, Tf-IDF weighting
# 2) Creates sentiment scores 
# Code Sourced From: https://rstudio-pubs-static.s3.amazonaws.com/132792_864e3813b0ec47cb95c7e1e2e2ad83e7.html



setwd("../")


#install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels', 'RWeka'))

library(tm)
library(SnowballC)
library(slam)
library(RWeka)

review.train <- read.csv("data/train/yelp_academic_dataset_review_train.csv",
                         stringsAsFactors = F)

review.test <- read.csv("data/test/yelp_academic_dataset_review_test.csv",
                        stringsAsFactors = F)
review.test$stars <- NA

review.all <- rbind(review.train, review.test)

#Removing unecessary columns
review.all <- subset(review.all, select = -c(X, type))


#Cleaning review text via Corpus
review_corpus <- Corpus(VectorSource(review.all$text))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus =  tm_map(review_corpus, stripWhitespace)



#Inspect
inspect(review_corpus[1])

#Dividing into Reviews = Rows, Columns = words/terms
#by term frequency
review_termfreq <- DocumentTermMatrix(review_corpus)
#review_termfreq <- DocumentTermMatrix(review_corpus, 
#                                      control = list(tokenize = TrigramTokenizer))
#review_termfreq <- removeSparseTerms(review_termfreq, sparse = 0.97)

#Using Tf-Idf to weight by relative importance to document, not by term frequency
review_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
# Removing terms (columns) that have sparsity > 98%. Basically only selecting "important" terms
review_tfidf <- removeSparseTerms(review_tfidf, sparse = 0.98)



review_tfidf


# Our own empirical most negative and positive words
emp.neg_words <- as.character(read.table("data/most_negative_words.txt", sep = ",", header = F, stringsAsFactors = F))
emp.neg_words <- trimws(emp.neg_words)

emp.pos_words <- as.character(read.table("data/most_positive_words.txt", sep = ",", header = F, stringsAsFactors = F))
emp.pos_words <- trimws(emp.pos_words)


#Sentiment Analysis: Positive and Negative Words
#Source: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
neg_words <- read.table("resources/negative-words.txt", header = F, 
                        stringsAsFactors = F)[, 1]
pos_words <- read.table("resources/positive-words.txt", header = F, 
                        stringsAsFactors = F)[, 1]


# Remove duplicate from neg_words and pos_words
neg_words <- neg_words[!(neg_words %in% emp.neg_words)]
pos_words <- pos_words[!(pos_words %in% emp.pos_words)]


#Based on number of matching terms in each sentiment corpus, 
#normalized by wordcount per document
wordcount.per <- row_sums(x = review_termfreq)

neg.score <- tm_term_score(review_termfreq, neg_words) 
pos.score <- tm_term_score(review_termfreq, pos_words) 

#Weighted for Empirical Most positive and negative words
emp.weight <- 2
emp.pos.score <- emp.weight * tm_term_score(x = review_termfreq, terms = emp.pos_words)
emp.neg.score <- emp.weight * tm_term_score(x = review_termfreq, terms = emp.neg_words)

# Total Sentiment Scores
review.all$pos.score <- (emp.pos.score + pos.score) / wordcount.per
review.all$neg.score <- (emp.neg.score + neg.score) / wordcount.per

# Total Output result: review.train + sentiment scores + relative importance matrix of words to document determined by tf-idf, with max 98% sparsity in term column

review.all.fe <- cbind(review.all, as.matrix(review_tfidf))

review.train.fe <- review.all.fe[!is.na(review.all.fe$stars), ]
review.test.fe <- review.all.fe[is.na(review.all.fe$stars), ]

save(review.test.fe, file = "data/clean/test/review_test_clean.RData")
save(review.train.fe, file = "data/clean/train/review_train_clean.RData")


