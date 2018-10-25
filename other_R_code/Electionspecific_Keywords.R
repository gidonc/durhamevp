#This file contains the code to identify election specific keywords from the OCR of all classified material
#Input: Corpus or articles from 19th century newspapers
#Output: Data.frame of general election violence specific keywords and keywords for each election

library(tidyverse)
library(dbplyr)

rm(list=ls())

killDbConnections()

#Get all the classified documents from the 10 keyword systematic search
alldocs<-get_document("all")
alldocs<-alldocs %>%
  dplyr::mutate(std_url = sub("download/", "", url))

docallocs<-get_allocation("all", allocation_type = "coding")

docallocs<-left_join(docallocs, alldocs, by=c("document_id"="id"))

classdocs<-filter(docallocs, coding_complete==1, electoralviolence_nature %in% c("false", "true"))
classdocs<-classdocs %>%
  dplyr::mutate(std_url = sub("download/", "", url)) %>%
  dplyr::mutate(unclass=0, classified=1) %>%
  dplyr::mutate(EV_article=ifelse(electoralviolence_nature=="true", 1, 0))

##----dataprep----
systematic_search_terms<-c("election",
                           "riot",
                           "disturbance",
                           "incident",
                           "police",
                           "husting",
                           "magistrate",
                           "party",
                           "rough",
                           "stone")
all_searches<-get_archivesearches()
systematic_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(search_text %in% systematic_search_terms)

results_systematic_searches<-get_archivesearchresults(archive_search_id = systematic_searches$id) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

is_classified <-results_systematic_searches$std_url %in% classdocs$std_url

classified_results<-results_systematic_searches[is_classified,]

systematic_class_docs<-classdocs[classdocs$std_url %in% classified_results$std_url,]
systematic_class_docs$fakeid<-1:nrow(systematic_class_docs)

#Create dataset of classified election violence datasets with election indicators based on publication date

class_ev_docs<-subset(systematic_class_docs, EV_article==1,
                      select=-fakeid)

class_ev_docs<-class_ev_docs %>%
  dplyr::mutate(elect_1832=ifelse(publication_date>lubridate::ymd("1832-01-01") & publication_date<lubridate::ymd("1833-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1835=ifelse(publication_date>lubridate::ymd("1835-01-01") & publication_date<lubridate::ymd("1835-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1837=ifelse(publication_date>lubridate::ymd("1837-01-01") & publication_date<lubridate::ymd("1837-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1841=ifelse(publication_date>lubridate::ymd("1841-01-01") & publication_date<lubridate::ymd("1841-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1847=ifelse(publication_date>lubridate::ymd("1847-01-01") & publication_date<lubridate::ymd("1847-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1852=ifelse(publication_date>lubridate::ymd("1852-01-01") & publication_date<lubridate::ymd("1852-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1857=ifelse(publication_date>lubridate::ymd("1857-01-01") & publication_date<lubridate::ymd("1857-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1859=ifelse(publication_date>lubridate::ymd("1859-01-01") & publication_date<lubridate::ymd("1859-12-31"), 1, 0)) %>%
  dplyr::mutate(elect_1865=ifelse(publication_date>lubridate::ymd("1865-01-01") & publication_date<lubridate::ymd("1865-12-31"), 1, 0))
class_ev_docs$fakeid<-1:nrow(class_ev_docs)

#Set Cutpoint for top keywords

cutpoint<-0.8

#Extract keywords using OCR of all classified documents

full_corpus<-quanteda::corpus(systematic_class_docs[c("fakeid", "classified", "EV_article", "ocr")], text_field="ocr")

full_dfm <- durhamevp::preprocess_corpus(full_corpus,stem=TRUE, min_termfreq=50, min_docfreq=50, max_termfreq=NULL, max_docfreq=NULL,remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",dfm_tfidf=FALSE)

ev_keywords<-durhamevp::nb_keywords(full_dfm, "EV_article")
ev_topkw<-as.data.frame(subset(ev_keywords[,1],ev_keywords[,3]>=cutpoint))
ev_topkw$evkw<-1
colnames(ev_topkw)<-c("keywords","EV keyword")

#Extract keywords that predict the different elections within the set of election violence articles

ev_corpus<-quanteda::corpus(class_ev_docs[c("fakeid", "classified", "elect_1832","elect_1835","elect_1837", "elect_1841", "elect_1852", "elect_1857", "elect_1859", "elect_1865", "ocr")], text_field="ocr")

ev_dfm <- durhamevp::preprocess_corpus(ev_corpus,stem=TRUE, min_termfreq=50, min_docfreq=50, max_termfreq=NULL, max_docfreq=NULL,remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",dfm_tfidf=FALSE)

kw_1832<-durhamevp::nb_keywords(ev_dfm, "elect_1832")
topkw_1832<-as.data.frame(subset(kw_1832[,1],kw_1832[,3]>=cutpoint))
topkw_1832$y <- 1
colnames(topkw_1832)<-c("keywords","1832")

kw_1835<-durhamevp::nb_keywords(ev_dfm, "elect_1835")
topkw_1835<-as.data.frame(subset(kw_1835[,1],kw_1835[,3]>=cutpoint))
topkw_1835$y <- 1
colnames(topkw_1835)<-c("keywords","1835")

kw_1837<-durhamevp::nb_keywords(ev_dfm, "elect_1837")
topkw_1837<-as.data.frame(subset(kw_1837[,1],kw_1837[,3]>=cutpoint))
topkw_1837$y <- 1
colnames(topkw_1837)<-c("keywords","1837")

kw_1841<-durhamevp::nb_keywords(ev_dfm, "elect_1841")
topkw_1841<-as.data.frame(subset(kw_1841[,1],kw_1841[,3]>=cutpoint))
topkw_1841$y <- 1
colnames(topkw_1841)<-c("keywords","1841")

kw_1852<-durhamevp::nb_keywords(ev_dfm, "elect_1852")
topkw_1852<-as.data.frame(subset(kw_1852[,1],kw_1852[,3]>=cutpoint))
topkw_1852$y <- 1
colnames(topkw_1852)<-c("keywords","1852")

kw_1857<-durhamevp::nb_keywords(ev_dfm, "elect_1857")
topkw_1857<-as.data.frame(subset(kw_1857[,1],kw_1857[,3]>=cutpoint))
topkw_1857$y <- 1
colnames(topkw_1857)<-c("keywords","1857")

kw_1859<-durhamevp::nb_keywords(ev_dfm, "elect_1859")
topkw_1859<-as.data.frame(subset(kw_1859[,1],kw_1859[,3]>=cutpoint))
topkw_1859$y <- 1
colnames(topkw_1859)<-c("keywords","1859")

kw_1865<-durhamevp::nb_keywords(ev_dfm, "elect_1865")
topkw_1865<-as.data.frame(subset(kw_1865[,1],kw_1865[,3]>=cutpoint))
topkw_1865$y <- 1
colnames(topkw_1865)<-c("keywords","1865")

#Collecting keywords in a table

topkw_1832_1865<-Reduce(function(x,y) merge(x=x, y=y, by="keywords",all=TRUE), list(ev_topkw,topkw_1832,topkw_1835,topkw_1837,topkw_1841,topkw_1852,topkw_1857,topkw_1859,topkw_1865))

write.csv(topkw_1832_1865,"/Users/patrick/Desktop/keywords_1832-65.csv")






