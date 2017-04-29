# InitialModel.R

library(randomForest)

# Loading Initial Data
load("data/clean/train/train_join.RData")
load("data/clean/test/test_join.RData")

# Splitting dataset by 80% train, 20% test (from training dataset)
train.ind <- base::sample(nrow(joined_train), size = 0.8 * nrow(joined_train))

train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]

# Random Forest Classifier
# Column Name (review.stars)
rf.model <- randomForest(formula = stars ~ ., data = train.set,
                         )


