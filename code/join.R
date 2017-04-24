# join.R
# Joins all datasets together (separates train and test)

setwd("~/Desktop/s154_project/data")


#Reading in all CSVs and saving into RData for easier reading in next time
b.train <- read.csv("train/yelp_academic_dataset_business_train.csv",
                    stringsAsFactors = F)
review.train <- read.csv("train/yelp_academic_dataset_review_train.csv",
                         stringsAsFactors = F)

tip <- read.csv("train/yelp_academic_dataset_tip.csv", stringsAsFactors = F)
user <- read.csv("train/yelp_academic_dataset_user.csv", stringsAsFactors = F)
checkin <- read.csv("train/yelp_academic_dataset_checkin.csv", stringsAsFactors = F)
save.image(file = "train/alltrain.RData")


load("train/alltrain.RData")

library(dplyr)

# Joining Review with Tip
all.df <- list(review = review.train, checkin = checkin, 
               business = b.train, tip = tip, user = user)
clean.df <- list()
for(i in 1:length(all.df)){
  df <- all.df[[i]]
  df <- df[ , -c(1, grep("type", colnames(df), ignore.case = T))]
  colnames(df) <- paste0(paste0(substr(names(all.df)[i], 1,1), "."), colnames(df))
  clean.df <- append(clean.df, list(df))
}

names(clean.df) <- c("review", "checkin", "business", "tip", "user")

#Joining on Review and Business
join1 <- dplyr::left_join(x = clean.df$review, y = clean.df$business, 
                          by = c("r.business_id" = "b.business_id"))


# Joining Tip with Business (not yet because of issues)
#join2 <- left_join(x = tip, y = b.train, by = "business_id")

# Joining Business with Checkin
join3 <- left_join(x = join1, y = clean.df$checkin, 
                   by = c("r.business_id" = "c.business_id"))

# Joining Checkin with User
join4 <- left_join(x = join3, y = clean.df$user, by = c("r.user_id" = "u.user_id"))


# Exporting joined data
save(join4, file = "train/train_join.RData")
