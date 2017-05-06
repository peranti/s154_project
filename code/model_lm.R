# load("~/stat154-dropbox/train_join.RData")
# load("~/stat154-dropbox/test_join.RData")

indices <- sample(1:nrow(joined_train), nrow(joined_train)*0.2)

joined_train2 <- joined_train[,-15]
joined_test2 <- joined_test[,-16]

for(i in 1:ncol(joined_train2)) {
  if(is.factor(joined_train2[,i]) & length(levels(joined_train2[,i])) < 2) {
    print(paste(colnames(joined_train2)[i], length(levels(joined_train2[,i]))))
  }
}

lm1 <- lm(stars ~ ., data = joined_train2, subset = -indices)

preds <- predict(lm1, joined_train2[indices,])

adjuster <- function(pred) {
  if(pred > 5) {
    return(5)
  } else if (pred < 0) {
    return(0)
  } else {
    return(pred)
  }
}

preds <- sapply(preds, adjuster)

MSE <- mean((preds - joined_train2$stars[indices])^2)

lm2 <- lm(stars ~ ., data = joined_train2)

lm.preds <- sapply(predict(lm2, joined_test2), adjuster)

names(lm.preds) <- joined_test2$business_id

save(lm.preds,file = "~/Dropbox/stat154-dropbox/lm_preds.RData")

