dropbox.path<-switch (Sys.info()["nodename"],
                      "DM-GIA-051"="D:/Dropbox",
                      "DM-GIA-055"="D:/Dropbox",
                      "GIDON-ASUS-15"="C:/Users/Gidon/Dropbox",
                      "GID-HOME-LENOVO"="C:/Users/Gidon/Dropbox")

project.path<-paste0(dropbox.path, "/ESRC Grant EV 19th Century")
match_data_path<-paste0(project.path, "/Code/database_interactivity/reporting/coder_scoring/data")


all_coding_matches<-extract_coding_matches()

uda<-find_agreement_from_matches(all_coding_matches, "user_doc")
eva<-find_agreement_from_matches(all_coding_matches, "ev_report")
ta<-find_agreement_from_matches(all_coding_matches, "tags")
atta<-find_agreement_from_matches(all_coding_matches, "attributes")

coding_agreement <- bind_rows(uda,
                              eva,
                              ta,
                              atta)


all_users<-get_user("all") %>%
  dplyr::select(id, first_name, last_name)

long_coding_agreements <- coding_agreement %>%
  tidyr::gather(which_user, user_id, user_id, user_id1) %>%
  dplyr::left_join(all_users, by=c("user_id"="id")) %>%
  tidyr::unite(fullname, first_name, last_name) %>%
  dplyr::mutate(stype = substr(match_type, 1, 2)) %>%
  tidyr::unite(variable, variable, stype)

write.csv(long_coding_agreements, paste0(match_data_path, "/long_coding_agreements.csv"))

nmm<-matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
              1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA),nrow=4)
