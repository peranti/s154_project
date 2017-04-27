#!/usr/bin/python

import sys
import csv
import re
import operator
from collections import defaultdict

TEXT_COLUMN = 4
STARS_COLUMN = 6

MIDDLE_STARS = 3

avg_stars = 0.0
total_rows = 0

if len(sys.argv) != 2:
	print 'Usage: %s review_csv_file' % sys.argv[0]
	exit(-1)


with open(sys.argv[1], 'rb') as csvfile:
	business_reader = csv.reader(csvfile)

	header = next(business_reader)
	# sanity checks	
	assert(header[TEXT_COLUMN] == 'text')
	assert(header[STARS_COLUMN] == 'stars')

	word_weights = defaultdict(lambda: 0)
	word_counts = defaultdict(lambda: 0)

	for row in business_reader:
		total_rows += 1
		text = row[TEXT_COLUMN]
		stars = int(row[STARS_COLUMN])
		avg_stars += stars
		assert(1.0 <= stars <= 5.0)

		# clean text
		text = text.lower() # convert to lowercase
		text = re.sub('[^a-z]', ' ', text) # remove non-ascii
		words = text.split()

		for word in words:
			word_weights[word] += stars - MIDDLE_STARS
			word_counts[word] += 1

avg_stars /= total_rows

MIN_TIMES_CUTOFF = 25

normalized_weights = dict()
for word in word_weights:
	if word_counts[word] >= MIN_TIMES_CUTOFF:
		normalized_weights[word] = word_weights[word] / float(word_counts[word])


sorted_weights = sorted(word_weights.items(), key=operator.itemgetter(1))
sorted_normalized = sorted(normalized_weights.items(), key=operator.itemgetter(1))

def top(n, positive=True):
	if positive:
		words = sorted_normalized[-n:]
	else:
		words = sorted_normalized[:n]
	return ', '.join([pair[0] for pair in words])

print 'avg_stars %s' % avg_stars
print 'Most negative words:'
print top(250, positive=False)
print 'Most positive words:'
print top(250, positive=True)
