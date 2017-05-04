# load("~/stat154-dropbox/train_join.RData")
# load("~/stat154-dropbox/test_join.RData")

library(e1071)

indices <- sample(1:nrow(joined_train), nrow(joined_train)*0.2)

joined_train2 <- joined_train[,-15]
joined_test2 <- joined_test[,-16]

svm1 <- svm(stars ~ ., data = joined_train2)
