library(data.table)
library(dplyr)
library(zoo)
library(tidyr)

setwd("../")
# business <- fread("data/train/yelp_academic_dataset_business_train.csv",
#                   data.table = FALSE)
# checkin <- fread("data/yelp_academic_dataset_checkin.csv", data.table = FALSE)
# review <- fread("data/train/yelp_academic_dataset_review_train.csv", data.table = FALSE)
# tip <- fread("data/yelp_academic_dataset_tip.csv", data.table = FALSE)
# user <- fread("data/yelp_academic_dataset_user.csv", data.table = FALSE)

# load("data/train/alltrain.RData")

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
  n <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")
  v <- ifelse(n %in% categories, 1, 0)
  names(v) <- n
  return(v)
}

business.categories.d <- array_parser(business, "categories", business.categories_fn)
business.categories <- data.frame(matrix(unlist(business.categories.d), nrow=length(business.categories.d), byrow=T))
colnames(business.categories) <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")

#### BUSINESS.HOURS

business.hours_fn <- function(hours) {
  hours <- paste(hours,"-",sep="")
  if(hours == "-") {
    rval <- rep(NA,23)
    names(rval) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                     "Mon_ct", "Tues_ct", "Wed_ct", "Thurs_ct", "Fri_ct", "Sat_ct", "Sun_ct",
                     "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs",
                     "total_hrs", "avg_hrs")
    return(rval)
  }
  parsed <- unlist(strsplit(hours, "y|-"))
  parsed.df <- data.frame(matrix(parsed, ncol=3, byrow=T), stringsAsFactors = F)
  names(parsed.df) <- c("day", "start", "end")
  
  time_completer <- function(time) {
    return(as.ITime(ifelse(time=="0:0", "23:59:59", time)))
  }
  
  time_subtracter <- function(start, end) {
    if(end >= start) {
      return(round(as.numeric(end - start)/3600))
    } else {
      return(round((as.numeric(as.ITime("23:59:59") - start) + as.numeric(end - as.ITime("0:00")))/3600))
    }
  }
  
  parsed.df <- parsed.df %>%
    transmute(day = paste(day, "y", sep=""), 
              start = time_completer(start),
              end = time_completer(end)) %>%
    mutate(hours = time_subtracter(start, end))
  # print(parsed.df)

  rvec <- rep(0,23)
  d <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
         "Mon_ct", "Tues_ct", "Wed_ct", "Thurs_ct", "Fri_ct", "Sat_ct", "Sun_ct",
         "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs",
         "total_hrs", "avg_hrs")
  names(rvec) <- d
  for(i in 1:7) {
    t <- sum(d[i] == parsed.df$day)
    rvec[i] <- ifelse(t > 0, 1, 0)
    rvec[i+7] <- t
    rvec[i+14] <- sum(parsed.df$hours[parsed.df$day == d[i]])
    rvec['total_hrs'] <- sum(parsed.df$hours)
    rvec['avg_hrs'] <- mean(parsed.df$hours)
  }
  return(rvec)
}

business.hours.d <- array_parser(business, "hours", business.hours_fn)
business.hours <- data.frame(matrix(unlist(business.hours.d), nrow=length(business.hours.d), byrow=T))
colnames(business.hours) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                              "Mon_ct", "Tues_ct", "Wed_ct", "Thurs_ct", "Fri_ct", "Sat_ct", "Sun_ct",
                              "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs",
                              "total_hrs", "avg_hrs")
business.hours_na.rm <- na.aggregate(business.hours)

#### CHECKIN.TIME

checkin.time_fn <- function(time) {
  parsed <- unlist(strsplit(time, ":|-"))
  parsed.df <- data.frame(matrix(parsed, ncol=3, byrow=T), stringsAsFactors = F)
  names(parsed.df) <- c("Day", "Hour", "Checkins")
  parsed.df$Hour <- as.numeric(parsed.df$Hour)
  parsed.df$Checkins <- as.numeric(parsed.df$Checkins)
  # parsed.df <- parsed.df %>%
  #   spread(key = Hour, value = Checkins)
  vec <- rep(0, 35)
  n <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",
         "Mon_bf", "Tue_bf", "Wed_bf", "Thu_bf", "Fri_bf", "Sat_bf", "Sun_bf",
         "Mon_l", "Tue_l", "Wed_l", "Thu_l", "Fri_l", "Sat_l", "Sun_l",
         "Mon_d", "Tue_d", "Wed_d", "Thu_d", "Fri_d", "Sat_d", "Sun_d",
         "Mon_ln", "Tue_ln", "Wed_ln", "Thu_ln", "Fri_ln", "Sat_ln", "Sun_ln")
  names(vec) <- n
  for(i in 1:7){
    vec[i] <- sum(parsed.df$Checkins[parsed.df$Day == n[i]])
    vec[i+7] <- sum(parsed.df$Checkins[parsed.df$Day == n[i] & between(parsed.df$Hour, 6, 9)])
    vec[i+14] <- sum(parsed.df$Checkins[parsed.df$Day == n[i] & between(parsed.df$Hour, 10, 14)])
    vec[i+21] <- sum(parsed.df$Checkins[parsed.df$Day == n[i] & between(parsed.df$Hour, 17, 21)])
    vec[i+28] <- sum(parsed.df$Checkins[parsed.df$Day == n[i] & (parsed.df$Hour < 2 | parsed.df$Hour > 23)])
  }
  return(vec)
}

checkin.time.d <- array_parser(checkin, "time", checkin.time_fn)
checkin.time <- data.frame(matrix(unlist(checkin.time.d), nrow=length(checkin.time.d), byrow=T))
colnames(checkin.time) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",
                            "Mon_bf", "Tue_bf", "Wed_bf", "Thu_bf", "Fri_bf", "Sat_bf", "Sun_bf",
                            "Mon_l", "Tue_l", "Wed_l", "Thu_l", "Fri_l", "Sat_l", "Sun_l",
                            "Mon_d", "Tue_d", "Wed_d", "Thu_d", "Fri_d", "Sat_d", "Sun_d",
                            "Mon_ln", "Tue_ln", "Wed_ln", "Thu_ln", "Fri_ln", "Sat_ln", "Sun_ln")