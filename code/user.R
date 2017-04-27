## User.R
## Parses User Related Attributes for Feature Engineering

setwd("../")

load("data/train/alltrain.RData")

# From Vaibhav
array_parser <- function(df, column, fn) {
  cleaned_col <- gsub("'|[[:space:]]|\\[|\\]","",df[,column])
  vector <- strsplit(cleaned_col,",")
  return(lapply(vector, fn))
}

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

#Output
save(user.features.df, file = "data/train/user.features.df.RData")
