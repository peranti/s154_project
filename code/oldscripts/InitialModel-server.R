# InitialModel.R
setwd("/home/stat")

library(randomForest)
library(caret)
library(xgboost)

# Loading Initial Data
load("stat154-dropbox/train_join.RData")

# Quick Data Cleaning
joined_train$n.activity <- as.numeric(as.character(joined_train$n.activity))
joined_train$stars <- as.factor(joined_train$stars)

# Splitting dataset by 80% train, 20% test (from training dataset)
# Preserves factor distribution of the y variable
set.seed(199)
train.ind <- createDataPartition(joined_train$stars, p = 0.8, list = F, times = 1)
#train.ind <- base::sample(nrow(joined_train), size = 0.8*nrow(joined_train))
train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]



######## 1. Random Forest Classifier ########
# Response Variable Column Name (stars)
rf.model <- randomForest(formula = stars ~ ., data = train.set)
#rf.model <- ranger(stars ~ ., data = train.set[ ,c(531:543,49)], write.forest = TRUE)

oob.mse <- rf.model$mse

predicted.stars <- predict(rf.model, data = test.set)

mse <- mean((test.set$stars - predicted.stars)^2)

######## 2. XGboost and parameter tuning via caret ########
library(doMC)
registerDoMC(cores = 6)
(l <- sapply(joined_train, function(x) is.factor(x)))
l <- sapply(train.set[, -which(leve==1)], function(x) is.factor(x))
m <- joined_train[, l]
leve <- sapply(1:ncol(m), function(x) return(nlevels(m[,x])))
rm.col <- colnames(m)[which(leve==1)]
rm.col <- which(colnames(train.set) == rm.col)
caret.ctrl <- trainControl(method = "cv", repeats = 1, number = 5, allowParallel = T,
                           classProbs = T, search = "grid")


caret.xgb.model <- caret::train(form = stars ~. , data = train.set[ ,-rm.col],
                                preProcess = "knnImpute", method = "xgbLinear", verbose = T)
