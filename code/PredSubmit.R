# PredSubmit.R
library(plyr)
library(dplyr)
library(randomForest)

# Server Data Directory
load("stat154-dropbox/test_join.RData")
load("stat154-dropbox/rfmodel.RData")
load('stat154-dropbox/xgb.class.model.RData')
load("stat154-dropbox/xgb.model.lin.RData")
load("stat154-dropbox/lm_preds.RData")
load('stat154-dropbox/bag_stars.RData')

#load("data/clean/test/test_join.RData")

bow_csv <- read.csv("stat154-dropbox/BagOfWords_averagedpredictions.csv",
                    stringsAsFactors = F)

models <- list("rf" = rf.model,"xgblin" = xgb.lin.model, "xgbclass" = xgb.class.model)

rev.ratings <- data.frame(business_id = joined_test$business_id)

for(i in 1:length(models)){
  
  test_predicted <- as.numeric(predict(models[[i]], newdata = joined_test))
  rev.ratings <- cbind(rev.ratings, test_predicted)
  cat(names(models[i]), "done..\n")
}

rev.ratings$lmodel <- lm.preds
rev.ratings$bag <- bag_predicted$stars

  
#Clipping 0 and 5 (bounding them)
rev.ratings[ ,c(2:ncol(rev.ratings))] <-
  apply(rev.ratings[ ,c(2:ncol(rev.ratings))], 2, function(x){
    x <- ifelse(x < 1, 1, x) 
    x <- ifelse(x > 5, 5, x)
    return(x)
  })

##### 1st submission - average all prediction results #####
rev.ratings$all.rating <- apply(rev.ratings, 1, function(x){
  return(mean(as.numeric(x[2:ncol(rev.ratings)])))
})

colnames(rev.ratings) <- c("business_id", names(models), "lmodel", "all.rating")

business_ratings = rev.ratings %>%
  dplyr::select(business_id, all.rating) %>%
  group_by(business_id) %>%
  summarise(stars = mean(all.rating))

write.csv(business_ratings, file = "stat154-dropbox/ensemble1.csv", 
          row.names = FALSE)


##### 2nd Submission - After removing top and bottom outliers #####
head(rev.ratings)
rev.ratings$all.rating <- apply(rev.ratings, 1, function(x){
  preds <- sort(as.numeric(x[2:ncol(rev.ratings)]))
  nonout.preds <- preds[2:(length(preds)-1)]
  return(mean(nonout.preds))
})

colnames(rev.ratings) <- c("business_id", names(models), "lmodel", "bag", "all.rating")

business_ratings = rev.ratings %>%
  dplyr::select(business_id, all.rating) %>%
  group_by(business_id) %>%
  summarise(stars = mean(all.rating))

#Weight 1/8 (less weight on primitive bow model)
#business_ratings <- left_join(business_ratings, bow_csv, by = "business_id")
#business_ratings$stars <- ((7/8) * business_ratings$stars.x + 
#                             (1/8) * business_ratings$stars.y)
#business_ratings <- business_ratings[ ,c(1, 4)]
write.csv(business_ratings, file = "stat154-dropbox/ensemble3.csv", 
          row.names = FALSE)
