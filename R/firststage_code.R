#This file contains the code to run the first stage
#Input: Corpus or articles from 19th century newspapers
#Output: Data.frame of keywords from naive bayes classifier

#The code requires that all functions in firststage_function.R can be called on

library(tidyverse)
library(gmodels)
library(caret)

rm(list=ls())

#One Time Connection to the Database (necesary for Patrick to connect to database)
#keyring::key_set(service="evp", user="data_writer")
#con<-evdb_connect(password_method = "keyring")

#Get candidate documents
classdoc<-get_classified_docs()

#Split document matrix in training and test sets
sets<-create_docsets(classdoc,size=500,seed=123) #assumes fakeid is field in classdoc sequentially numbering documents

#Create corpus
train<-quanteda::corpus(sets$training[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")
test<-quanteda::corpus(sets$test[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")

#Preprocess corpus and create DFM (without n-grams); preprocessing choices can be adapted in function
train_nograms_dfm<-preprocess_corpus(train, stem=FALSE)
test_nograms_dfm<-preprocess_corpus(test, stem=FALSE)

#Preprocess corpus and create DFM (with specific n-grams); preprocessing choices can be adapted in function
ev_grams<-list(c('election','riot'),c('election','disturbance'),c('election','incident'))

train_grams_dfm<-preprocess_sgrams(train, wseq=ev_grams, stem=FALSE)
test_grams_dfm<-preprocess_sgrams(test, wseq=ev_grams, stem=FALSE)

#Extract keywords from naive bayes classifier run on training dfm
kw_nograms<-nb_keywords(train_nograms_dfm, classvar="EV_article")
head(kw_nograms, 10)

kw_grams<-nb_keywords(train_grams_dfm, classvar="EV_article")
head(kw_grams, 10)
