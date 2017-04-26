# join.R
# Joins all datasets together (separates train and test)

library(dplyr)

######## TRAINING DATA ########

#Reading in all CSVs and saving into RData for easier reading in next time
b.train <- read.csv("data/train/yelp_academic_dataset_business_train.csv",
                    stringsAsFactors = F)
review.train <- read.csv("data/train/yelp_academic_dataset_review_train.csv",
                         stringsAsFactors = F)

tip <- read.csv("data/yelp_academic_dataset_tip.csv", stringsAsFactors = F)
user <- read.csv("data/yelp_academic_dataset_user.csv", stringsAsFactors = F)
checkin <- read.csv("data/yelp_academic_dataset_checkin.csv", stringsAsFactors = F)


######## TESTING DATA ########
b.test <- read.csv("data/test/yelp_academic_dataset_business_test.csv", 
                    stringsAsFactors = F)
review.test <- read.csv("data/test/yelp_academic_dataset_review_test.csv",
                          stringsAsFactors = F)

train_data <- list(review = review.train, checkin = checkin, business = b.train, tip = tip, user = user)
train_cleaned <- list()
for(i in 1:length(train_data)){
  df <- train_data[[i]]
  df <- df[ , -c(1, grep("type", colnames(df), ignore.case = T))] #removing first row and the
  colnames(df) <- paste0(paste0(substr(names(train_data)[i], 1,1), "."), colnames(df))
  train_cleaned <- append(train_cleaned, list(df))
}
names(train_cleaned) <- c("review", "checkin", "business", "tip", "user")

#Joining reviews with business on the business id
rev_bus <- dplyr::left_join(x = train_cleaned$review, y = train_cleaned$business, 
                          by = c("r.business_id" = "b.business_id"))

 
#Joining reviews and businesses with the checkins for each business
rb_check <- dplyr::left_join(x = rev_bus, y = train_cleaned$checkin, by = c("r.business_id" = "c.business_id"))

#Joining previous with user
rbc_user <- dplyr::left_join(x = rb_check, y = train_cleaned$user, by = c("r.user_id" = "u.user_id"))

#joining in the previous with tips for a business
rbcu_tip = dplyr::left_join(x = rbc_user, y = train_cleaned$tip, by = c("r.business_id" = "t.business_id", "r.user_id" = "t.user_id"))


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

counter = 0
sapply(joined_train, function(x) {
  if (sum(is.na(x)) != 0) {
    counter = counter + 1
  }
})
counter







