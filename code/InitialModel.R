# InitialModel.R

library(randomForest)
library(dplyr)

# Loading Initial Data
load("data/clean/train/train_join.RData")
load("data/clean/test/test_join.RData")

# Splitting dataset by 80% train, 20% test (from training dataset)
train.ind <- base::sample(nrow(joined_train), size = 0.8 * nrow(joined_train))


train.set <- joined_train[train.ind, ]
test.set <- joined_train[-train.ind, ]

# Random Forest Classifier
# Response Variable Column Name (stars)
rf.model <- randomForest(formula = as.factor(stars) ~ ., data = train.set) #might not work, I had to add B_id so I could sort

oob.mse <- rf.model$mse

test.set$stars = as.factor(test.set$stars)
predicted.stars <- predict(rf.model, newdata = test.set)
mse <- mean((as.numeric(test.set$stars) - as.numeric(predicted.stars))^2)

save(rf.model, file = "data/clean/train/rfmodel.RData")

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

