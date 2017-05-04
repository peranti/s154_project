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

(l <- sapply(joined_train, function(x) is.factor(x)))
m <- joined_train[, l]
leve <- sapply(1:ncol(m), function(x) return(nlevels(m[,x])))
rm.col <- colnames(m)[which(leve==1)]
rm.col <- which(colnames(joined_train) == rm.col)

joined_train <- joined_train[, -rm.col]
train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]




######## 2. XGboost Linear and parameter tuning via caret ########

library(doMC)
registerDoMC(cores = 6)
caret.ctrl <- trainControl(method = "none",allowParallel = T, search = "grid",
                           preProcOptions = "medianImpute")
xgb.lin.model <- caret::train(form = stars ~. , data = train.set,
                              method = "xgbLinear", verbose = T,
                              trControl = caret.ctrl)
preds <- predict(xgb.lin.model, newdata = test.set)
xgb.lin.train.mse <- mean((test.set$stars - preds)^2)

xgb.lin.model <- caret::train(form = stars ~. , data = joined_train,
                              method = "xgbLinear", verbose = T,
                              trControl = caret.ctrl)

save(list = c("xgb.lin.model", "xgb.lin.train.mse"), 
     file = "stat154-dropbox/xgb.model.lin.RData")




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

xgb.class.train.mse <- mean((as.numeric(as.character(test.set$stars)) - 
                               as.numeric(as.character(preds)))^2)

xgb.class.model <-
  caret::train(form = stars ~. , data = joined_train,
               method = "xgbTree", verbose = T, 
               trControl = caret.ctrl)

save(list = c("xgb.class.model", "xgb.class.train.mse"), 
     file = "stat154-dropbox/xgb.class.model.RData")



