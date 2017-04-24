library(data.table)
library(dplyr)

setwd("../")
# business <- fread("data/train/yelp_academic_dataset_business_train.csv", 
#                   data.table = FALSE)
# checkin <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_checkin.csv", data.table = FALSE)
# review <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_review_train.csv", data.table = FALSE)
# tip <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_tip.csv", data.table = FALSE)
# user <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_user.csv", data.table = FALSE)

load("data/train/alltrain.RData")

## EDA to explore review+tip join issue

r <- review %>%
  select(business_id, user_id)
t <- tip %>%
  select(business_id, user_id)
j <- left_join(r, t, by = c("business_id" = "business_id", "user_id" = "user_id"))

r. <- r %>%
  group_by(business_id, user_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
t. <- t %>%
  group_by(business_id, user_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


######################## ONLY VAIBHAV BELOW #############################

array_parser <- function(df, column, fn) {
  return(lapply(strsplit(gsub("'|[[:space:]]|\\[|\\]","",df[,column]),","), fn))
}

#### BUSINESS.CATEGORIES

business.categories_fn <- function(categories) {
  v <- rep(0, 6)
  n <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")
  names(v) <- n
  for(i in 1:length(n)) {
    if(n[i] %in% categories) {
      v[i] <- 1
    }
  }
  return(v)
}

business.categories.d <- array_parser(business, "categories", business.categories_fn)
business.categories <- data.frame(matrix(unlist(q), nrow=length(q), byrow=T))
colnames(business.categories) <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")

#### BUSINESS.HOURS

business.hours_fn <- function(hours) {
  
}



