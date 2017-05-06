# PredSubmit.R
library(plyr)
library(dplyr)
library(randomForest)
library(kernlab)
# Server Data Directory
load("stat154-dropbox/test_join.RData")
load("stat154-dropbox/rfmodel.RData")
load('stat154-dropbox/xgb.class.model.RData')
load("stat154-dropbox/xgb.model.lin.RData")
load("stat154-dropbox/lm_preds.RData")
load("stat154-dropbox/svm.lin.model.RData")
load('stat154-dropbox/bag_stars.RData')
load("stat154-dropbox/test.svm.preds.RData")
rm("svm.preds")

bow_csv <- read.csv("stat154-dropbox/BagOfWords_averagedpredictions.csv",
                    stringsAsFactors = F)

models <- list("rf" = rf.model,"xgblin" = xgb.lin.model, "xgbclass" = xgb.class.model)

rev.ratings <- data.frame(business_id = joined_test$business_id)

for(i in 1:length(models)){
  
  test_predicted <- as.numeric(predict(models[[i]], newdata = joined_test))
  rev.ratings <- cbind(rev.ratings, test_predicted)
  cat(names(models[i]), "done..\n")
}

rev.ratings$svm <- test.svm.preds
rev.ratings$lmodel <- lm.preds
#rev.ratings$bag <- bag_predicted$stars

  
#Clipping 1 and 5 (bounding them)
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

colnames(rev.ratings) <- c("business_id", names(models), "svm",
                           "lmodel", "all.rating")

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

colnames(rev.ratings) <- c("business_id", names(models), "svm",
                           "lmodel", "all.rating")

business_ratings = rev.ratings %>%
  dplyr::select(business_id, all.rating) %>%
  group_by(business_id) %>%
  summarise(stars = mean(all.rating))

#Weight 1/8 (less weight on primitive bow model)
#business_ratings <- left_join(business_ratings, bow_csv, by = "business_id")
#business_ratings$stars <- ((7/8) * business_ratings$stars.x + 
#                             (1/8) * business_ratings$stars.y)
#business_ratings <- business_ratings[ ,c(1, 4)]
write.csv(business_ratings, file = "stat154-dropbox/ensemble6.csv", 
          row.names = FALSE)




####### 3rd submission: Weights based on MSE + No olypmic diving (remove top and bottom outlier)
head(rev.ratings)

mses <- c(0.1970, 0.6010, 0.6980,  0.7599546, 0.7196)
weights <- (1/mses) / sum(1/mses)

rev.ratings$all.rating <- 
  apply(rev.ratings, 1, function(x){
  return(sum(weights * as.numeric(x[2:ncol(rev.ratings)])))
})
colnames(rev.ratings) <- c("business_id", names(models), "svm",
                           "lmodel", "all.rating")

business_ratings = rev.ratings %>%
  dplyr::select(business_id, all.rating) %>%
  group_by(business_id) %>%
  summarise(stars = mean(all.rating))

# FINAL: ROUNDING THE RATINGS
classes <- seq(1,5,by=0.5)
round.ratings <- sapply(business_ratings$stars,function(x){
  diffs <- abs(x - classes)
  return(classes[which.min(diffs)])
})

business_ratings$stars <- round.ratings

write.csv(business_ratings, file = "stat154-dropbox/ensemble5.csv", 
          row.names = FALSE)
