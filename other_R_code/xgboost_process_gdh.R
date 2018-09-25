# Machine Learning 1841
candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()

initial_1841_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1841-06-25"), archive_date_start<lubridate::ymd("1841-07-25")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1841<-get_archivesearchresults(archive_search_id = initial_1841_searches$id)
dim(res_i_1841)

res_i_1841 <- res_i_1841 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb<-classifier_selection_description(candocs, res_i_1841, classifier_type = "xgboost")

dim(select_descript_xgb)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$status!="1"|is.na(xgb_cand_docs$status), c("status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb[,c("url", "description")], "checkit.csv", row.names=FALSE)








# Machine Learning 1832
candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()

initial_1832_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1832-12-01"), archive_date_start<lubridate::ymd("1833-02-01")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1832<-get_archivesearchresults(archive_search_id = initial_1832_searches$id)
dim(res_i_1832)

res_i_1832 <- res_i_1832 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb2<-classifier_selection_description(candocs, res_i_1832, classifier_type = "xgboost")

dim(select_descript_xgb2)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb2)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$status!="1"|is.na(xgb_cand_docs$status), c("status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb2[,c("url", "description")], "checkit2.csv", row.names=FALSE)




# Machine Learning 1835
candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()


initial_1835_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1835-01-05"), archive_date_start<lubridate::ymd("1835-02-07")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1835<-get_archivesearchresults(archive_search_id = initial_1835_searches$id)
dim(res_i_1835)

res_i_1835 <- res_i_1835 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb3<-classifier_selection_description(candocs, res_i_1835, classifier_type = "xgboost")

dim(select_descript_xgb3)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb3)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$status!="1"|is.na(xgb_cand_docs$status), c("status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb3[,c("url", "description")], "checkit3.csv", row.names=FALSE)


# Machine Learning 1837
candocs<-get_candidate_documents(g_status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()

initial_1837_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1837-07-19"), archive_date_start<lubridate::ymd("1837-08-21")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1837<-get_archivesearchresults(archive_search_id = initial_1837_searches$id)
dim(res_i_1837)

res_i_1837 <- res_i_1837 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb4<-classifier_selection_description(candocs, res_i_1837, classifier_type = "xgboost")

dim(select_descript_xgb4)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb4)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$g_status!="1"|is.na(xgb_cand_docs$status), c("status", "g_status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb4[sample(nrow(select_descript_xgb4)),c("url", "description")], "checkit4.csv", row.names=FALSE)






# Machine Learning 1847
candocs<-get_candidate_documents(g_status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()

initial_1847_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1847-07-27"), archive_date_start<lubridate::ymd("1847-08-29")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1847<-get_archivesearchresults(archive_search_id = initial_1847_searches$id)
dim(res_i_1847)

res_i_1847 <- res_i_1847 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb5<-classifier_selection_description(candocs, res_i_1847, classifier_type = "xgboost")

dim(select_descript_xgb5)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb5)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$g_status!="1"|is.na(xgb_cand_docs$status), c("status", "g_status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb5[sample(nrow(select_descript_xgb5)),c("url", "description")], "checkit5.csv", row.names=FALSE)



# Machine Learning 1852
candocs<-get_candidate_documents(g_status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)

all_searches<-get_archivesearches()

initial_1852_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1852-07-03"), archive_date_start<lubridate::ymd("1852-08-05")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))

res_i_1852<-get_archivesearchresults(archive_search_id = initial_1852_searches$id)
dim(res_i_1852)

res_i_1852 <- res_i_1852 %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb6<-classifier_selection_description(candocs, res_i_1852, classifier_type = "xgboost")

dim(select_descript_xgb6)

xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb6)


knitr::kable(head(xgb_cand_docs[xgb_cand_docs$g_status!="1"|is.na(xgb_cand_docs$status), c("status", "g_status", "description")]))

xgb_cand_docs$status<-1

xgb_cand_docs$status_writer<-"xgboost_description"



to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", Sys.time(), ".csv"))
csv_filename<-gsub(":", "_", csv_filename)
write.csv(to_csv, file=csv_filename, row.names=FALSE)

getwd()



# Write different sort of csv - see the descriptions the classifier is basing its judgement on
write.csv(select_descript_xgb5[sample(nrow(select_descript_xgb6)),c("url", "description")], "checkit6.csv", row.names=FALSE)
