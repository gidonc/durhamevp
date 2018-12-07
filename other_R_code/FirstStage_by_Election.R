#This file contains the code to run the first stage for different years to predict additional keywords
#Input: Corpus or articles from 19th century newspapers
#Output: Data.frame of keywords from naive bayes classifier by election year 1832-1910

#The code requires that all functions in firststage_function.R can be called on

library(tidyverse)
library(gmodels)
library(caret)
library(dbplyr)

rm(list=ls())

killDbConnections()

#Get all systematically downloaded documents
alldocs<-get_document("all")
alldocs<-alldocs %>%
  dplyr::mutate(std_url = sub("download/", "", url))

docallocs<-get_allocation("all", allocation_type = "coding")

docallocs<-left_join(docallocs, alldocs, by=c("document_id"="id"))

docs<-docallocs %>%
  dplyr::mutate(std_url = sub("download/", "", url)) %>%
  dplyr::mutate(classified=ifelse(coding_complete==1&electoralviolence_nature %in% c("true", "false"), 1, 0), unclassified=ifelse(classified==1, 0, 1)) %>%
  dplyr::mutate(EV_article=ifelse(electoralviolence_nature=="true", 1, ifelse(electoralviolence_nature=="false", 0, NA_integer_))) %>%
  as_tibble()

#Prepare the data
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

is_downloaded<-results_systematic_searches$std_url %in% docs$std_url

downloaded_results<-results_systematic_searches[is_downloaded,]

systematic_downloaded_docs<-docs[docs$std_url %in% downloaded_results$std_url,]

#Create datasets with classified documents for each election year
#1832
systematic_downloaded_docs_1832<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1832-01-01"), publication_date<lubridate::ymd("1833-12-31"))

systematic_downloaded_docs_1832$fakeid<-1:nrow(systematic_downloaded_docs_1832)

kw_fcm_1832<-run_firststage_fcm(docs=systematic_downloaded_docs_1832, docid="fakeid", textvar="description",
                           min_termfreq=20, min_docfreq=20,
                           initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                           cpoint2=0.75)

#1835
systematic_downloaded_docs_1835<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1835-01-01"), publication_date<lubridate::ymd("1836-12-31"))

systematic_downloaded_docs_1835$fakeid<-1:nrow(systematic_downloaded_docs_1835)

kw_fcm_1835<-run_firststage_fcm(docs=systematic_downloaded_docs_1835, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1837
systematic_downloaded_docs_1837<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1837-01-01"), publication_date<lubridate::ymd("1838-12-31"))

systematic_downloaded_docs_1837$fakeid<-1:nrow(systematic_downloaded_docs_1837)

kw_fcm_1837<-run_firststage_fcm(docs=systematic_downloaded_docs_1837, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1841
systematic_downloaded_docs_1841<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1841-01-01"), publication_date<lubridate::ymd("1842-12-31"))

systematic_downloaded_docs_1841$fakeid<-1:nrow(systematic_downloaded_docs_1841)

kw_fcm_1841<-run_firststage_fcm(docs=systematic_downloaded_docs_1841, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1847
systematic_downloaded_docs_1847<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1847-01-01"), publication_date<lubridate::ymd("1848-12-31"))

systematic_downloaded_docs_1847$fakeid<-1:nrow(systematic_downloaded_docs_1847)

kw_fcm_1847<-run_firststage_fcm(docs=systematic_downloaded_docs_1847, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1852
systematic_downloaded_docs_1852<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1852-01-01"), publication_date<lubridate::ymd("1853-12-31"))

systematic_downloaded_docs_1852$fakeid<-1:nrow(systematic_downloaded_docs_1852)

kw_fcm_1852<-run_firststage_fcm(docs=systematic_downloaded_docs_1852, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1857
systematic_downloaded_docs_1857<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1857-01-01"), publication_date<lubridate::ymd("1858-12-31"))

systematic_downloaded_docs_1857$fakeid<-1:nrow(systematic_downloaded_docs_1857)

kw_fcm_1857<-run_firststage_fcm(docs=systematic_downloaded_docs_1857, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1859
systematic_downloaded_docs_1859<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1859-01-01"), publication_date<lubridate::ymd("1860-12-31"))

systematic_downloaded_docs_1859$fakeid<-1:nrow(systematic_downloaded_docs_1859)

kw_fcm_1859<-run_firststage_fcm(docs=systematic_downloaded_docs_1859, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#1865
systematic_downloaded_docs_1865<-systematic_downloaded_docs %>%
  filter(publication_date>lubridate::ymd("1865-01-01"), publication_date<lubridate::ymd("1866-12-31"))

systematic_downloaded_docs_1865$fakeid<-1:nrow(systematic_downloaded_docs_1865)

kw_fcm_1865<-run_firststage_fcm(docs=systematic_downloaded_docs_1865, docid="fakeid", textvar="description",
                                min_termfreq=20, min_docfreq=20,
                                initialkw=c("elect", "riot", "disturb", "incid","polic","hust","magistr","party","rough","stone"),
                                cpoint2=0.75)

#Merging the keyword output together
kw_1832_1865<-Reduce(function(x,y) merge(x=x, y=y, by="keywords",all=TRUE), list(kw_fcm_1832,kw_fcm_1835,kw_fcm_1837, kw_fcm_1841, kw_fcm_1847, kw_fcm_1852, kw_fcm_1857, kw_fcm_1859, kw_fcm_1859, kw_fcm_1865))

colnames(kw_1832_1865)<-c("keywords","abs_1832", "rel_1832","abs_1835", "rel_1835","abs_1837", "rel_1837","abs_1841", "rel_1841","abs_1847", "rel_1847","abs_1852", "rel_1852","abs_1857", "rel_1857","abs_1859", "rel_1859","abs_1865", "rel_1865")

kw_1832_1865<-kw_1832_1865[order(-kw_1832_1865$rel_1832,-kw_1832_1865$rel_1835,-kw_1832_1865$rel_1837,-kw_1832_1865$rel_1841,-kw_1832_1865$rel_1847,-kw_1832_1865$rel_1852,-kw_1832_1865$rel_1857,-kw_1832_1865$rel_1859,-kw_1832_1865$rel_1865),]

write.csv(kw_1832_1865,"/Users/patrick/Desktop/keywordsuggestions_1832-65.csv")

