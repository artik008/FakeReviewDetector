#!/usr/local/bin/python3
# -*- coding: utf-8 -*-
# import nltk
from nltk import word_tokenize
from nltk import sent_tokenize
from nltk import regexp_tokenize

import pymorphy2

import sys

sent_tokenizer = (lambda s: sent_tokenize(s))

punkt_tokenizer   = (lambda s: regexp_tokenize(s, r'[a-zA-Z0-9\s]', gaps=True))
punkt_tokenizer_2 = (lambda s: regexp_tokenize(s, r'[,\.\?!"$%<>/\-\–]', gaps=False))

morph = pymorphy2.MorphAnalyzer()

only_word_tokenizer = (lambda s: regexp_tokenize(s, r'[,\.\?!"\s0-9$%\(\)\-\–]', gaps=True))


words = only_word_tokenizer(sys.argv[1])
sents = sent_tokenize(sys.argv[1])
puncts = punkt_tokenizer_2(sys.argv[1])

speechParts = {}
for word in words:
  scannedWord = str(morph.parse(word)[0].tag.POS)
  if scannedWord in speechParts.keys():
    speechParts[scannedWord] += 1
  else:
    speechParts.update({scannedWord:1})

punctCount = {}
for punct in puncts:
  if punct == '"':
    punct = '\\"'
  if punct in punctCount.keys():
    punctCount[punct] += 1
  else:
    punctCount.update({punct:1})

total_word_length = 0
for word in words:
  total_word_length += len(word)

words_count = len(words)

sent_count = len(sents)

result = "ProcessedTextInfo {" +\
           "ptAvgWordsInSent = " + str(words_count/sent_count) + "," +\
           "ptWordCount = " + str(words_count) + "," +\
           "ptAvgWordLength = " + str(total_word_length/words_count) + "," +\
           "ptPunctuation = ["  

punctPairs = []

for punctPair in punctCount.items():
  punctPairs.append("(\"" + str(punctPair[0]) + "\"," + str(punctPair[1]) + ")")

result += ','.join(punctPairs)

result += "]" + "," +\
          "ptSpeechParts = ["

speechPartsPairs = []

for spPair in speechParts.items():
  speechPartsPairs.append("(\"" + str(spPair[0]) + "\"," + str(spPair[1]) + ")")

result += ','.join(speechPartsPairs)

result += "]}"

print(result, end='')




