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
colnames(review.train.fe)[which(colnames(review.train.fe) == "text")] = "r.text"
colnames(tip)[which(colnames(tip) == "text")] = "t.text"

train_data <- list(review = review.train.fe, checkin = checkin.clean, business = business.train.clean, tip = tip, user = user.clean)
train_cleaned <- list()
for(i in 1:length(train_data)){
  df <- train_data[[i]]
  #colnames(df) <- paste0(paste0(substr(names(train_data)[i], 1,1), "."), colnames(df)) #after, try removing this too, 
  train_cleaned <- append(train_cleaned, list(df))
}
names(train_cleaned) <- c("review", "checkin", "business", "tip", "user")

#Joining reviews with business on the business id
rev_bus <- dplyr::left_join(x = train_cleaned$business, y = train_cleaned$review, 
                          by = c("business_id" = "business_id"))

 
#Joining reviews and businesses with the checkins for each business
rb_check <- dplyr::left_join(x = rev_bus, y = train_cleaned$checkin, by = c("business_id" = "business_id"))

#Joining previous with user
rbc_user <- dplyr::left_join(x = rb_check, y = train_cleaned$user, by = c("user_id" = "user_id"))

#joining in the previous with tips for a business
rbcu_tip = dplyr::left_join(x = rbc_user, y = train_cleaned$tip, by = c("business_id" = "business_id", "user_id" = "user_id",
                                                                        "date" = "date"))


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

######## TESTING DATA ########
load("data/clean/test/business_test_clean.RData") #business.test.clean
load("data/clean/test/review_test_clean.RData") #review.test.fe


#Cleaning it up
colnames(review.test.fe)[which(colnames(review.test.fe) == "text")] = "r.text"
test_data <- list(review = review.test.fe, checkin = checkin.clean, business = business.test.clean, tip = tip, user = user.clean)
test_cleaned <- list()
for(i in 1:length(test_data)){
  df <- test_data[[i]]
  test_cleaned <- append(test_cleaned, list(df))
}
names(test_cleaned) <- c("review", "checkin", "business", "tip", "user")

#Joining reviews with business on the business id
rev_bus_test <- dplyr::left_join(x = test_cleaned$business, y = test_cleaned$review, 
                            by = c("business_id" = "business_id"))


#Joining reviews and businesses with the checkins for each business
rb_check_test <- dplyr::left_join(x = rev_bus_test, y = test_cleaned$checkin, by = c("business_id" = "business_id"))

#Joining previous with user
rbc_user_test <- dplyr::left_join(x = rb_check_test, y = test_cleaned$user, by = c("user_id" = "user_id"))

#joining in the previous with tips for a business
rbcu_tip_test = dplyr::left_join(x = rbc_user_test, y = test_cleaned$tip, by = c("business_id" = "business_id", "user_id" = "user_id",
                                                                        "date" = "date"))


#### CLEANING ##### 
joined_test = rbcu_tip_test

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


# Cleaning up the names of both test and train
neuter_columns <- function(joined_data) {
  joined_data = subset(joined_data, select=-c(is_open, longitude, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, Mon_hrs, Tues_hrs,
                                              Wed_hrs, Thurs_hrs, Fri_hrs, Sat_hrs, Sun_hrs, street.x, lot.x, street.y,Wed_bf, Thu_bf,
                                              Mon_l, Fri_bf, Sat_bf, Wed_l, Tue_l, Sun_bf, Fri_l,Sat_l,Thu_l,Wed_d,Sun_l, Mon_d, Tue_d,
                                              Thu_d,Sun_d,Sat_d, Fri_d, Wed_ln, Tue_ln,Thu_ln, Mon_ln, Fri_ln,Sun_ln,Sat_ln,
                                              cool.y.y, type, stars.y, stars.y.y))
  }


final_renaming <- function(joined_data) {
  colnames(joined_data)[which(colnames(joined_data) == "stars.x")] = "stars"
  return(colnames(joined_data))
}

joined_test = neuter_columns(joined_test)
joined_train = neuter_columns(joined_train)

colnames(joined_test) =  final_renaming(joined_test)
colnames(joined_train) =  final_renaming(joined_train)


#Saving The Data
save(joined_test, file = "data/clean/test/test_join.RData")
save(joined_train, file = "data/clean/train/train_join.RData")






