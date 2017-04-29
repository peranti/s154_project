library(data.table)
library(plyr)
library(dplyr)
library(zoo)
library(tidyr)
library(magrittr)

# setwd("../")
business <- fread("data/train/yelp_academic_dataset_business_train.csv", data.table = FALSE)
checkin <- fread("data/yelp_academic_dataset_checkin.csv", data.table = FALSE)
review <- fread("data/train/yelp_academic_dataset_review_train.csv", data.table = FALSE)
tip <- fread("data/yelp_academic_dataset_tip.csv", data.table = FALSE)
user <- fread("data/yelp_academic_dataset_user.csv", data.table = FALSE)
business.test <- fread("data/test/yelp_academic_dataset_business_test.csv", data.table = FALSE)
review.test <- fread("data/test/yelp_academic_dataset_business_test.csv", data.table = FALSE)
# load("data/train/alltrain.RData")

## array_parser digests columns that are in array-string form and turns them into usable features when fed a helper function

array_parser <- function(df, column, fn) {
  return(lapply(strsplit(gsub("'|[[:space:]]|\\[|\\]","",df[,column]),","), fn))
}

#### BUSINESS

stars.test <- rep(0, nrow(business.test))
business.test <- cbind(business.test, stars.test)[,-2]

business <- cbind(select(business, -stars), business$stars)
names(business)[ncol(business)] <- "stars"
names(business.test)[ncol(business.test)] <- "stars"

business.combined <- rbind(business, business.test)

business.cleaner <- function(business) {
  ###### BUSINESS.HOURS
  
  business.hours_fn <- function(hours) {
    hours <- paste(hours,"-",sep="")
    if(hours == "-") {
      rval <- rep(NA,14)
      names(rval) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                       "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs")
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
    
    rvec <- rep(0,14)
    d <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
           "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs")
    names(rvec) <- d
    for(i in 1:7) {
      rvec[i] <- ifelse(sum(d[i] == parsed.df$day) > 0, 1, 0)
      rvec[i+7] <- sum(parsed.df$hours[parsed.df$day == d[i]])
    }
    return(rvec)
  }
  
  business.hours.d <- array_parser(business, "hours", business.hours_fn)
  business.hours <- data.frame(matrix(unlist(business.hours.d), nrow=length(business.hours.d), byrow=T))
  colnames(business.hours) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                                "Mon_hrs", "Tues_hrs", "Wed_hrs", "Thurs_hrs", "Fri_hrs", "Sat_hrs", "Sun_hrs")
  business.hours_na.rm <- na.aggregate(business.hours)
  
  ###### BUSINESS.ATTRIBUTES
  
  # Cleaning Attributes
  atts <- business$attributes
  atts <- gsub("\\}|\\[|\\]", "", gsub(",\\s\".{3,15}:", ",", atts))
  atts <- gsub("('|\"|\\{|\\})", "", atts)
  
  #Splitting by column name (attribute) and value
  atts.list <- lapply(atts, function(x) unlist(strsplit(x, split = ",")))
  
  # Making DF function
  make_df <- function(at){
    # Input: Character vector with both column name and value per element of vector
    # Output: data-frame structure of all the elements (column-value pair)
    split.vals <- strsplit(at, split = ":")
    col.nm <- c()
    vals <- c()
    if(length(split.vals) == 0){
      return(NA)
    }
    else{
      for(i in 1:length(split.vals)){
        col.nm <- append(col.nm, trimws(split.vals[[i]][1]))
        vals <- append(vals, trimws(split.vals[[i]][2]))
      }
      final.df <- t(data.frame(vals))
      colnames(final.df) <- col.nm
      rownames(final.df) <- NULL
      return(final.df)
    }
  }
  
  # Filling in dataframe columns
  atts.df <- data.frame()
  for(i in 1:length(atts.list)){
    atts.df <- rbind.fill(atts.df, as.data.frame(make_df(atts.list[[i]])))
    # cat("Processed:", i, "of", length(atts.list), "businesses..\n")
  }
  
  # Weird column
  atts.df$`make_df(atts.list[[i]])` <- NULL
  
  # Currently all columns in Factor.
  # Converting T-F columns to numeric
  for(i in 1:ncol(atts.df)){
    if( sum( !(atts.df[, i] %in% c("True", "False", NA)) ) == 0){
      atts.df[, i] <- as.numeric(as.logical(atts.df[, i]))
    }
  }
  
  business.attributes <- atts.df
  
  ###### BUSINESS.CATEGORIES
  
  business.categories_fn <- function(categories) {
    n <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")
    v <- ifelse(n %in% categories, 1, 0)
    names(v) <- n
    return(v)
  }
  
  business.categories.d <- array_parser(business, "categories", business.categories_fn)
  business.categories <- data.frame(matrix(unlist(business.categories.d), nrow=length(business.categories.d), byrow=T))
  colnames(business.categories) <- c("Mexican", "Restaurants", "Coffee&Tea", "Food", "Pizza", "Chinese")
  
  ######### final business cleaning
  
  business.clean <- cbind(business, business.hours_na.rm, business.attributes, business.categories)
  business.clean <- business.clean %>%
    select(-V1, -neighborhood, -hours, -address, -attributes, -categories, -type)
  print(names(business.clean))
  to.remove <- c()
  nr <- nrow(business.clean)
  for(i in 25:78) {
    if(sum(is.na(business.clean[,i]))/nr > 0.5) {
      to.remove <- c(to.remove, i)
    }
  }
  
  business.clean <- business.clean[,-to.remove]
  business.clean[25:65] <- lapply(business.clean[25:65], factor)
  business.clean$NoiseLevel <- relevel(relevel(relevel(business.clean$NoiseLevel, "loud"), "average"), "quiet")
  return(business.clean)
}

business.combined.clean <- business.cleaner(business.combined)

business.train.clean <- business.combined.clean[1:nrow(business),]
business.test.clean <- business.combined.clean[(nrow(business)+1):nrow(business.combined.clean),]

#### CHECKIN

###### CHECKIN.TIME

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

checkin.clean <- cbind(checkin, checkin.time)
checkin.clean %<>% select(-V1, -time, -type)

#### USERS

elite.years.col <- function(years){
  # Input: Character Vector of Years
  # Output: Data Frames making Years as columns
  # Note: column names sequenced by all.years, will add this later
  output.df <- matrix(ncol = length(all.years))
  output.df[1, ] <- as.numeric(all.years %in% years)
  return(output.df)
}

#Number of elite years and friends
user$n.elite <- unlist(array_parser(df = user, column = "elite", fn = length))
user$n.friend <- unlist(array_parser(df = user, column = "friends", fn = length))

## Years Attributes
splitted.years <- strsplit(gsub("'|[[:space:]]|\\[|\\]","", user[,"elite"]),",")
all.years <- unique(na.omit(as.numeric(unlist(splitted.years))))

years.col.pre <- array_parser(df = user, column = "elite", fn = elite.years.col)

years.df <- matrix(nrow = nrow(user), ncol = length(all.years))
for(i in 1:length(years.col.pre)){
  years.df[i, ] <- years.col.pre[[i]]
  cat("Processed:", i, "of", length(years.col.pre), "users..\n")
}

years.df <- as.data.frame(years.df)
colnames(years.df) <- all.years

#Total number of reactions received by user
user$n.react <- apply(user[ , grepl("compliment", colnames(user))], 1, sum)

#Total number of reactions *given* by the user
give.react.types <- c("useful", "funny", "cool")
user$n.activity <- apply(user[ ,colnames(user) %in% give.react.types], 1, sum)

#Finally joining the years indicator DF with original user DF
user.features.df <- cbind(user, years.df)

user.features.df$yelping_since <- as.IDate(user.features.df$yelping_since)

user.clean <- user.features.df %>%
  select(-V1, -elite, -type, -user_id, -friends, -name)
user.clean[23:35] <- lapply(user.clean[23:35], factor)

### saving all tables

save(business.train.clean, file = "data/clean/train/business_train_clean.Rdata")
save(business.test.clean, file = "data/clean/test/business_test_clean.Rdata")
save(checkin.clean, file = "data/clean/checkin_clean.Rdata")
save(user.clean, file = "data/clean/user_clean.Rdata")
