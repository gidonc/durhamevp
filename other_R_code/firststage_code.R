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
con<-evdb_connect(password_method = "keyring")

#Create clean document set for the 1832 election to run first stage function
classdocs<-durhamevp::get_classified_docs()

classdocs<-classdocs %>%
  dplyr::mutate(std_url = sub("download/", "", url)) %>%
  dplyr::mutate(unclass=0, classified=1)

##----initial.1832.results----
all_searches<-get_archivesearches()
initial_1832_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1832-01-01"), archive_date_start<lubridate::ymd("1832-12-31")) %>%
  filter(search_text %in% c("election riot", "election incident", "election disturbance"))

# a number of precisely duplicated searches here which return precisely duplicated results - we could handle this later on but here probably simplest to just use three distinct ones
# election riot - id 73
# election disturbance - id 81
# election incident - id 85
res_i_1832<-get_archivesearchresults(archive_search_id = c(73, 81, 85)) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

is_classified <-res_i_1832$std_url %in% classdocs$std_url

unclass_i_1832 <- res_i_1832[!is_classified,] %>%
  mutate(unclass=1, classified=0)

all_docs<- bind_rows(classdocs,
                     unclass_i_1832)
all_docs$fakeid<-1:nrow(all_docs)

#Run the first stage function focusing on change of predictability
kw_nbchng<-run_firststage_nbchng(docs=all_docs,min_termfreq=50, min_docfreq=50)
kw_nbchng

#Run the first stage function focusing on feature co-ouccerence
kw_fcm<-run_firststage_fcm(docs=all_docs,min_termfreq=50, min_docfreq=50, cpoint2=0.85)
kw_fcm

#DEVELOPMENT OF FIRST STAGE FCM
full_corpus<-quanteda::corpus(all_docs[c("fakeid", "classified", "EV_article", "description")], text_field="description")
full_dfm <- durhamevp::preprocess_corpus(full_corpus, stem=TRUE, remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE,
                                         min_termfreq=20, min_docfreq = 20, termfreq_type="count", docfreq_type="count")
topfeatures(full_dfm)
nfeat(full_dfm)

initialkw<-c("elect", "riot", "disturb","incident")

class_dfm<-quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, "classified")==1)
class_nb <- quanteda::textmodel_nb(class_dfm, y=quanteda::docvars(class_dfm, "EV_article"), prior="uniform")
keywords1<-durhamevp::nb_keywords(class_dfm, "EV_article")

S_dfm <- quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, "classified")==0)
quanteda::docvars(S_dfm, "T")<-predict(class_nb, newdata = S_dfm, type="class")
keywords2<-nb_keywords(S_dfm, "T")
topkw2<-subset(keywords2[,1],keywords2[,3]>=0.8)

full_fcm<-fcm(full_dfm)
pred_fcm<-fcm_select(full_fcm, pattern=c(initialkw,topkw2), selection="keep", valuetyp="fixed")
kw_fcm<-quanteda::convert(pred_fcm, to="matrix")
kw_fcm<-kw_fcm[rownames(kw_fcm)%in%initialkw,]
kw_fcm<-kw_fcm[,colnames(kw_fcm)%in%topkw2]
kw_fcm
result<-sort(apply(kw_fcm, 2,sum))
result


##----OLD CODE----

#Get candidate documents
classdoc<-get_classified_docs()

#Split document matrix in training and test sets
sets<-create_docsets(classdoc,size=500,seed=123) #assumes fakeid is field in classdoc sequentially numbering documents

#Create corpus
train<-quanteda::corpus(sets$training[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")
test<-quanteda::corpus(sets$test[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")

#Preprocess corpus and create DFM (without n-grams); preprocessing choices can be adapted in function
train_nograms_dfm<-preprocess_corpus(train, stem=FALSE, min_termfreq = 20, min_docfreq = 5)
test_nograms_dfm<-preprocess_corpus(test, stem=FALSE, min_termfreq = 20, min_docfreq = 5)

#Preprocess corpus and create DFM (with specific n-grams); preprocessing choices can be adapted in function
ev_grams<-list(c('election','riot'),c('election','disturbance'),c('election','incident'))

train_grams_dfm<-preprocess_sgrams(train, wseq=ev_grams, stem=FALSE, min_termfreq = 20, min_docfreq = 5)
test_grams_dfm<-preprocess_sgrams(test, wseq=ev_grams, stem=FALSE, min_termfreq = 20, min_docfreq = 5)

#Extract keywords from naive bayes classifier run on training dfm
kw_nograms<-nb_keywords(train_nograms_dfm, classvar="EV_article", distribution = "multinomial")
head(kw_nograms, 10)
tail(kw_nograms, 50)

kw_grams<-nb_keywords(train_grams_dfm, classvar="EV_article", distribution = "multinomial")
head(kw_grams, 10)
tail(kw_grams, 50)

riot_dfm<-quanteda::dfm_(train_grams_dfm, "riot")
quanteda::docvars(train_grams_dfm[as.logical(train_grams_dfm[,"elector"]==1),], "EV_article")

dfm_boolean<-quanteda::dfm_weight(train_grams_dfm, scheme="boolean")

kw_boolean<-nb_keywords(train_nograms_dfm, classvar="EV_article", distribution="Bernoulli")
head(kw_boolean, 10)
tail(kw_nograms, 50)
quanteda::textmodel_nb(dfm_boolean, quanteda::docvars(dfm_boolean, "EV_article"), distribution="multinomial")


##----King et al algorithm attempt----

search_set<- get_candidate_documents(cand_document_id = 2000:5000)
search_set$set<-0

ev_set<-classdoc[classdoc$EV_article==1,]
ev_set$set<-1


all_docs<-dplyr::bind_rows(search_set,
                            ev_set)
all_docs<-tibble::rowid_to_column(all_docs, "fakeid2")


ev_sample<-dplyr::sample_n(dplyr::filter(all_docs, set==1), 500)
search_sample<-dplyr::sample_n(dplyr::filter(all_docs, set==0), 500)

king_train<-dplyr::bind_rows(search_sample,
                             ev_sample)
#train_nograms_dfm<-preprocess_corpus(king_train, stem=FALSE, min_termfreq = 20, min_docfreq = 5)
#test_nograms_dfm<-preprocess_corpus(test, stem=FALSE, min_termfreq = 20, min_docfreq = 5)

# R = reference set (of election violence articles)
# S = search set [unlabelled]
# T = election violence articles in S

# 1. partition S into T and its complement (S\T)
#

train<-quanteda::corpus(king_train[,c("fakeid2", "ocr", "set")], text_field = "ocr")
#test<-quanteda::corpus(sets$test[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")

#Preprocess corpus and create DFM (without n-grams); preprocessing choices can be adapted in function
train_nograms_dfm<-preprocess_corpus(train, stem=FALSE, min_termfreq = 20, min_docfreq = 5)
#test_nograms_dfm<-preprocess_corpus(test, stem=FALSE, min_termfreq = 20, min_docfreq = 5)

#Preprocess corpus and create DFM (with specific n-grams); preprocessing choices can be adapted in function
#ev_grams<-list(c('election','riot'),c('election','disturbance'),c('election','incident'))

train_grams_dfm<-preprocess_sgrams(train, wseq=ev_grams, stem=FALSE, min_termfreq = 20, min_docfreq = 20)
#test_grams_dfm<-preprocess_sgrams(test, wseq=ev_grams, stem=FALSE, min_termfreq = 20, min_docfreq = 5)

#Extract keywords from naive bayes classifier run on training dfm
kw_nograms<-nb_keywords(train_nograms_dfm, classvar="set")
head(kw_nograms, 50)
tail(kw_nograms, 50)

kw_grams<-nb_keywords(train_grams_dfm, classvar="EV_article")
head(kw_grams, 10)
tail(kw_grams, 50)


smash<-contains_words(king_train, " rough", results_col="set")

ev_article<-classdoc[classdoc$EV_article==1,]
ev_corpus<-quanteda::corpus(ev_article[,c("fakeid", "ocr", "EV_article")], text_field = "ocr")
ev_dfm<-preprocess_corpus(ev_corpus, min_docfreq = 200, stem=FALSE)
ev_dfm==0

kw_nograms<-nb_keywords(ev_dfm, classvar="EV_article")

ev_docfreq<-quanteda::docfreq(ev_dfm)
ev_docfreq[order(ev_docfreq, decreasing = TRUE)]
sum((ev_dfm[,"mr"]==0 & ev_dfm[,"one"]==0 & ev_dfm[,"day"]==0 & ev_dfm[,"riot"]==0 & ev_dfm[,"police"]==0 & ev_dfm[,"last"]==0& ev_dfm[,"night"]==0))

d2<-quanteda::dfm_subset(ev_dfm, ev_dfm[,"mr"]==0 & ev_dfm[,"one"]==0 & ev_dfm[,"day"]==0 & ev_dfm[,"riot"]==0 & ev_dfm[,"police"]==0 & ev_dfm[,"last"]==0 & ev_dfm[,"night"]==0)

ev_docfreq<-quanteda::docfreq(d2)

toks<-quanteda::tokens(the_corpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                       remove_separators = TRUE, remove_hyphens = TRUE)
