## Business Attributes.R
## Parses Attributes column (array) in Business CSV data

load("../data/train/alltrain.RData")

library(plyr)
# Cleaning Attributes
atts <- b.train$attributes
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
  atts.df <- rbind.fill(atts.df, as.data.frame(make_df(atts.list[[i]])
                                               )
                        )
  cat("Processed:", i, "of", length(atts.list), "businesses..\n")
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

b.atts.df <- cbind(b.train , atts.df)
save(b.atts.df, file = "../data/train/b.atts.df.RData")
