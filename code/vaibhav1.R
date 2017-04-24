library(data.table)

business <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_business_train.csv", data.table = FALSE)
checkin <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_checkin.csv", data.table = FALSE)
review <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_review_train.csv", data.table = FALSE)
tip <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_tip.csv", data.table = FALSE)
user <- fread("/Users/vaibhav/Documents/Year3_Junior/Semester 2/Stat 154/project/data/yelp_academic_dataset_user.csv", data.table = FALSE)
