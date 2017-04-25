library(data.table)
library(dplyr)
library(zoo)

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
  cleaned_col <- gsub("'|[[:space:]]|\\[|\\]","",df[,column])
  vector <- strsplit(cleaned_col,",")
  return(lapply(vector, fn))
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


