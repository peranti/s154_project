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
#rf.model <- randomForest(formula = as.factor(stars) ~ ., data = train.set) #might not work, I had to add B_id so I could sort

### Classification MSE
test.set$stars = as.factor(test.set$stars)
predicted.stars <- predict(rf.model, newdata = test.set)
mse <- mean((as.numeric(test.set$stars) - as.numeric(predicted.stars))^2)

save(rf.model, file = "data/clean/train/rfmodel.RData")

load("data/clean/test/test_join.RData")
load("data/clean/train/rfmodel.RData")

business_names = joined_test$business_id
test_predicted <- predict(rf.model, newdata = joined_test)
test_predicted = data.frame(business_names, test_predicted)
colnames(test_predicted) <- c("business_id", "stars")
test_predicted$stars = as.numeric(test_predicted$stars)

save(test_predicted, file = "data/clean/train/rf_stars.RData")


#now filtering to get predictions

business_ratings = test_predicted %>%
  select(business_id, stars) %>%
  group_by(business_id) %>%
  summarise(stars = mean(stars))

write.csv(business_ratings, file = "results/rf_first.csv", row.names = FALSE)


### Random Forest Regression ### 
test.set$stars = as.numeric((as.character(test.set$stars)))
predicted.stars <- predict(rf.model, newdata = test.set)
mse <- mean((as.numeric(test.set$stars) - as.numeric(predicted.stars))^2)

rf.model <- randomForest(formula = as.numeric((as.character(stars))) ~ ., data = train.set) #this should give us regression
rf_reg_model <- rf.model

##### Baggings

library(ipred)
bag.model <- bagging(formula = as.factor(stars) ~ ., data = train.set) 
save(bag.model, file = "data/clean/train/bagModel.RData")


my_bag_predict_train = predict(bag.model,newdata=test.set)
pred_stars<- as.numeric(my_bag_predict_train)
actual_stars<- test.set$stars
agreement.Vector_bag<-(actual_stars == pred_stars)
length.Test.Vector_bag<-length(actual_stars)
misClassif_bag<-1-sum(agreement.Vector_bag)/length.Test.Vector_bag
misClassif_bag

#making stars DF for Jong
business_names = joined_test$business_id
bag_predicted <- predict(bag.model, newdata = joined_test)
bag_predicted = data.frame(business_names, bag_predicted)
colnames(bag_predicted) <- c("business_id", "stars")
bag_predicted$stars = as.numeric(bag_predicted$stars)

mse <- mean((as.numeric(test.set$stars) - as.numeric(pred_stars))^2)


save(bag_predicted, file = "data/clean/train/bag_stars.RData")
