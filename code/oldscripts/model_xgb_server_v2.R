# InitialModel.R
#setwd("/home/stat")

library(randomForest)
library(caret)
library(xgboost)

# Loading Initial Data
load("stat154-dropbox/train_join.RData")

# Quick Data Cleaning
joined_train$n.activity <- as.numeric(as.character(joined_train$n.activity))

# Splitting dataset by 80% train, 20% test (from training dataset)
# Preserves factor distribution of the y variable
set.seed(199)
train.ind <- createDataPartition(joined_train$stars, p = 0.8, list = F, times = 1)
#train.ind <- base::sample(nrow(joined_train), size = 0.8*nrow(joined_train))
train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]



######## 1. Random Forest Classifier ########
# Response Variable Column Name (stars)
# rf.model <- randomForest(formula = stars ~ ., data = train.set)
# #rf.model <- ranger(stars ~ ., data = train.set[ ,c(531:543,49)], write.forest = TRUE)
# 
# oob.mse <- rf.model$mse
# 
# predicted.stars <- predict(rf.model, data = test.set)
# 
# mse <- mean((test.set$stars - predicted.stars)^2)


######## 2. XGboost Linear and parameter tuning via caret ########

(l <- sapply(joined_train, function(x) is.factor(x)))
m <- joined_train[, l]
leve <- sapply(1:ncol(m), function(x) return(nlevels(m[,x])))
rm.col <- colnames(m)[which(leve==1)]
rm.col <- which(colnames(train.set) == rm.col)

joined_train <- joined_train[, -rm.col]
train.set <- train.set[ ,-rm.col]
test.set <- test.set[ , -rm.col]

library(doMC)
registerDoMC(cores = 6)
caret.ctrl <- trainControl(method = "none",allowParallel = T, search = "grid",
                           preProcOptions = "medianImpute")
xgb.lin.model <- caret::train(form = stars ~. , data = train.set,
                              method = "xgbLinear", verbose = T,
                              trControl = caret.ctrl)

preds <- predict(xgb.lin.model, newdata = test.set)

xgb.lin.mse <- mean((test.set$stars - preds)^2)

save(xgb.lin.model, file = "stat154-dropbox/xgb.model.lin.RData")



load("data/clean/test/test_join.RData")

business_names = joined_test$business_id
test_predicted <- predict(rf.model, newdata = joined_test)
test_predicted = data.frame(business_names, test_predicted)
colnames(test_predicted) <- c("business_id", "stars")
test_predicted$stars = as.numeric(test_predicted$stars)


#now filtering to get predictions

business_ratings = test_predicted %>%
  select(business_id, stars) %>%
  group_by(business_id) %>%
  summarise(stars = mean(stars))

write.csv(business_ratings, file = "results/rf_first.csv", row.names = FALSE)




######## 2. XGboost Classification and parameter tuning via caret ########
# Changing as Factor
joined_train$stars <- factor(joined_train$stars, 
                             levels = c("1", "2", "3", "4", "5"))
train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]

library(doMC)
registerDoMC(cores = 6)
caret.ctrl <- trainControl(method = "none",allowParallel = T, search = "random",
                           preProcOptions = "medianImpute")
xgb.class.model <- 
  caret::train(form = stars ~. , data = train.set,
               method = "xgbTree", verbose = T, 
               trControl = caret.ctrl)

preds <- predict(xgb.class.model, newdata = test.set)

xgb.class.mse <- mean((as.numeric(as.character(test.set$stars)) - 
                         as.numeric(as.character(preds)))^2)

save(xgb.class.model, file = "stat154-dropbox/xgb.class.model.RData")



