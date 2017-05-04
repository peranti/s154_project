import pandas as pd 
import math
import re
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import random
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier

# Data contains 116,474 rows and 11 fields
data = pd.read_csv("../data/yelp_academic_dataset_review_train.csv", \
          header=0, delimiter=",")

num_reviews = len(data)

################################
######## Data Cleaning #########
################################

def remove_punctuation(review):
  letters_only = re.sub("[^a-zA-Z']",  # The pattern to search for
                        " ",           # The pattern to replace it with
                        review)        # The text to search
  return letters_only

def tokenize(review):
  lowercase_only = review.lower()
  words = lowercase_only.split()
  return words

# Remove stop words like "a" and "the" that carry little sentiment
def remove_stop_words(words):
  words = [w for w in words if not w in set(stopwords.words("english"))]
  return words

# Group words like "messages", "message", and "messaging" as one word
def lemmatize(words):
  wordnet_lemmatizer = WordNetLemmatizer()
  lemmatized_words = [wordnet_lemmatizer.lemmatize(word) for word in words]
  return lemmatized_words

def review_to_words(review):
  return remove_stop_words(tokenize(remove_punctuation(review)))


################################
#### Bag of Words Training #####
################################

vectorizer = CountVectorizer(analyzer = "word",   \
                             tokenizer = None,    \
                             preprocessor = None, \
                             stop_words = None,   \
                             max_features = 5000) 

# Randomly choose 80% of reviews for training set
train = data.sample(frac=0.8)
num_train = len(train)

clean_train_reviews = []
i = 0
for text in train["text"]:
  if (i+1) % 1000 == 0:
    print "Review %d of %d\n" % (i+1, num_train)
  clean_review_list = review_to_words(text)
  clean_review = " ".join(clean_review_list)
  clean_train_reviews.append(clean_review)
  i += 1

train_data_features = vectorizer.fit_transform(clean_train_reviews)
train_data_features = train_data_features.toarray()

# Initialize a Random Forest classifier with 100 trees
forest = RandomForestClassifier(n_estimators = 100) 

# Fit the forest to the training set, using the bag of words as 
# features and the star labels as the response variable
forest = forest.fit(train_data_features, train["stars"])

print "Sucessfully created random forest"


################################
##### Bag of Words Testing #####
################################

test = data.loc[~data["text"].isin(train["text"])]
num_test = len(test)

clean_test_reviews = []
i = 0
for text in test["text"]:
  if (i+1) % 1000 == 0:
    print "Review %d of %d\n" % (i+1, num_test)
  clean_review_list = review_to_words(text)
  clean_review = " ".join(clean_review_list)
  clean_test_reviews.append(clean_review)
  i += 1

test_data_features = vectorizer.transform(clean_test_reviews)
test_data_features = test_data_features.toarray()

result = forest.predict(test_data_features)

# Copy the results to a pandas dataframe with the mean squared error
output = pd.DataFrame(data={"id": test["business_id"],
                            "predicted_stars": result,
                            "actual_stars": test["stars"], 
                            "mse": (result - test["stars"])**2},
                      columns=["id", "predicted_stars", "actual_stars", "mse"])

output.to_csv("../data/BagOfWords_model.csv", index=False)

print "Wrote results to BagOfWords_model.csv"
