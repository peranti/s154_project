# Predicting Star Ratings of Yelp Restaurants
- Project Members: Jong Ha Lee, Vaibhav Rammoorthy, Bryan Alcorn, Ying Luo, Paul Bramsen
- [Detailed Report](https://github.com/rollonbears234/s154_project/blob/master/report/stat154_finalreport.pdf)

## Project Description
Yelp has anonymously open-sourced some of their data on restaurants and businesses. For our project, we sought to predict the star ratings of restaurants on Yelp using a wide range of features. These included business attributes (hours of business, etc.), check-in count, and, most importantly, a set of bag-of-words predictors that allow us to parse reviews for specific keywords via Natural Language Processing. We prepared and joined the different datasets and then implemented an ensemble model that calculated a truncated mean of the predictions of 7 distinct models to result in our final predictions, which achieved a MSE of 0.32494.

## Timeline
1. 04/21 - Exploratory Data Analysis, understanding the data
2. 04/28 - Data Cleaning and Preparation, data importing to R
3. 04/30 - Cross Validation and testing multiple models (pick best one)
4. 05/05 - Report completed

## Data
1. **business** dataset consists of restaurants and its attributes such as hours of operation, parking availability, credit card acceptance, etc.
2. **review** dataset consists of both review text as well as some ratings metrics (i.e. how many other users marked the review cool, funny, or useful)
3. **checkin** dataset consists of hourly checkins to restaurants over a monthly period.
4. **user** dataset consists of user data and their attributes such as number of reviews written, elite status, etc.
5. **tip** dataset consists of different helpful tips Yelpers wrote about restaurants.

## Computing Resource: Google Cloud Platform Virtual Machine Instance
- Run Debian: 4 dual core Intel Xeon CPUs @ 2.30GHz cores (8 cores total) and 52 GB of memory
- Setup and connect GitHub repo to VM RStudio for auto-sync of files after Git commits

## Exploratory Data Analysis
- looking at distribution of star reviews across restaurants
- most positive and negative words based on reviews
- correlation between specific names (i.e. waiter/waitress names) and review rating

## Processing and Cleaning
- Conversion of string variables to factorized/categorical variables
- Removing stop words and white spaces
- Text-parser to convert dictionary-formatted column values to unique columns
- Joining data

## Feature Engineering
- Identifying most positive and negative words across all reviews
- Tokenized bag-of-words matrix, and weighting of word sentiment (positive or negative) based on sentiment analysis and corpi
- tf-idf weighting measure to reduce high-dimensionality by removing words unimportant to reviews
- Numerical feature engineering, finding hidden variables and values from raw columns

## Predictive Modeling and Ensembling
- Models utilized: Linear Regression, Random Forest, Bagging, Boosting, Support Vector Machines
- Ensembling: Weigh based on testing MSE segmented from training set, and use truncated-mean of models' star review outputs

