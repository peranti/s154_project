# join.R
# Joins all datasets together (separates train and test)

library(dplyr)

######## TRAINING DATA ########

#Reading in all CSVs and saving into RData for easier reading in next time
load("data/clean/train/business_train_clean.RData") #business.train.clean
load("data/clean/train/review_train_clean.RData") #review.train.fe
load("data/clean/checkin_clean.RData") #checkin.clean
load("data/clean/user_clean.RData") #user.clean
tip <- read.csv("data/yelp_academic_dataset_tip.csv", stringsAsFactors = F)
tip <- tip[,-1]

#Cleaning it up
train_data <- list(review = review.train.fe, checkin = checkin.clean, business = business.train.clean, tip = tip, user = user.clean)
train_cleaned <- list()
for(i in 1:length(train_data)){
  df <- train_data[[i]]
  colnames(df) <- paste0(paste0(substr(names(train_data)[i], 1,1), "."), colnames(df)) #after, try removing this too, 
  train_cleaned <- append(train_cleaned, list(df))
}
names(train_cleaned) <- c("review", "checkin", "business", "tip", "user")

#Joining reviews with business on the business id
rev_bus <- dplyr::left_join(x = train_cleaned$business, y = train_cleaned$review, 
                          by = c("b.business_id" = "r.business_id"))

 
#Joining reviews and businesses with the checkins for each business
rb_check <- dplyr::left_join(x = rev_bus, y = train_cleaned$checkin, by = c("b.business_id" = "c.business_id"))

#Joining previous with user
rbc_user <- dplyr::left_join(x = rb_check, y = train_cleaned$user, by = c("r.user_id" = "u.user_id"))

#joining in the previous with tips for a business
rbcu_tip = dplyr::left_join(x = rbc_user, y = train_cleaned$tip, by = c("b.business_id" = "t.business_id", "r.user_id" = "t.user_id",
                                                                        "r.date" = "t.date"))


#### CLEANING ##### 
joined_train = rbcu_tip

#func for mode, ignores nyll --- from Stack Overflow
Mode <- function(x) { 
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

#Cleaning up the tip text
na.t.test = is.na(joined_train$t.text)
joined_train$t.text[na.t.test] = "NO TIP"

num_predictors_init = length(colnames(joined_train))
for (i in 1:num_predictors_init) {
  na.index = is.na(joined_train[,i])
  
  #filling integers with average
  if (is.numeric(joined_train[,i])) {
    this_mean = mean(joined_train[,i], na.rm = TRUE)
    joined_train[,i][na.index] = this_mean
  }
  
  #filling characters with mode
  if (is.character(joined_train[,i])) {
    this_mode = Mode(joined_train[,i])
    joined_train[,i][na.index] = this_mode
  }
}

#Saving Data
save(joined_train, file = "data/clean/train/train_join.RData")


######## TESTING DATA ########
load("data/clean/test/business_test_clean.RData") #business.test.clean
load("data/clean/test/review_test_clean.RData") #review.test.fe


#Cleaning it up
test_data <- list(review = review.test.fe, checkin = checkin.clean, business = business.test.clean, tip = tip, user = user.clean)
test_cleaned <- list()
for(i in 1:length(test_data)){
  df <- test_data[[i]]
  colnames(df) <- paste0(paste0(substr(names(test_data)[i], 1,1), "."), colnames(df)) #after, try removing this too, 
  test_cleaned <- append(test_cleaned, list(df))
}
names(test_cleaned) <- c("review", "checkin", "business", "tip", "user")

#Joining reviews with business on the business id
rev_bus_test <- dplyr::left_join(x = test_cleaned$business, y = test_cleaned$review, 
                            by = c("b.business_id" = "r.business_id"))


#Joining reviews and businesses with the checkins for each business
rb_check_test <- dplyr::left_join(x = rev_bus_test, y = test_cleaned$checkin, by = c("b.business_id" = "c.business_id"))

#Joining previous with user
rbc_user_test <- dplyr::left_join(x = rb_check_test, y = test_cleaned$user, by = c("r.user_id" = "u.user_id"))

#joining in the previous with tips for a business
rbcu_tip_test = dplyr::left_join(x = rbc_user_test, y = test_cleaned$tip, by = c("b.business_id" = "t.business_id", "r.user_id" = "t.user_id",
                                                                        "r.date" = "t.date"))


#### CLEANING ##### 
joined_test = rbcu_tip_test

#func for mode, ignores nyll --- from Stack Overflow
Mode <- function(x) { 
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

#Cleaning up the tip text
na.t.test = is.na(joined_train$t.text)
joined_train$t.text[na.t.test] = "NO TIP"

num_predictors_init = length(colnames(joined_train))
for (i in 1:num_predictors_init) {
  na.index = is.na(joined_train[,i])
  
  #filling integers with average
  if (is.numeric(joined_train[,i])) {
    this_mean = mean(joined_train[,i], na.rm = TRUE)
    joined_train[,i][na.index] = this_mean
  }
  
  #filling characters with mode
  if (is.character(joined_train[,i])) {
    this_mode = Mode(joined_train[,i])
    joined_train[,i][na.index] = this_mode
  }
}

# #Just doing a check
# counter = 0
# sapply(joined_train, function(x) {
#   if (sum(is.na(x)) != 0) {
#     counter = counter + 1
#   }
# 
# })
# counter

#Saving Data
save(joined_train, file = "data/clean/train/test_join.RData")





