
all_allocations<-durhamevp::get_allocation() %>%
  dplyr::filter(status=="COMPLETED")


for_analysis<-all_allocations %>%
  dplyr::group_by(document_id, status=="COMPLETED") %>%
  dplyr::tally() %>%
  dplyr::filter(n>1) %>%
  dplyr::left_join(all_allocations)

# create unique pairs of respondents 

coder_pairs <- for_analysis %>%
  dplyr::filter(user_id>6) %>%
  dplyr::filter(id>1800, allocation_type=="coding", status=="COMPLETED") %>%
  #dplyr::filter(id>1800, allocation_type=="coding") %>%
  dplyr::group_by(document_id) %>%
  tidyr::expand(user_id, user_id) %>%
  dplyr::filter(user_id < user_id1) %>%
  tibble::rowid_to_column("pair_no")

aa<-coder_pairs %>%
  tidyr::gather(which_user, user_id, -pair_no, -document_id) %>%
  dplyr::left_join(all_allocations) %>%
  tidyr::gather(variable, value, -pair_no, -document_id, -which_user, -user_id, -id) %>%
  dplyr::select(-user_id, -id) %>%
  tidyr::spread(which_user, value) %>%
  dplyr::filter(!variable %in% c("allocated_by", "allocation_date", "allocation_type", "ideal_coding_comments", "score", "status", "article_date_man_verify", "assigned_at", "difficulty_ranking", "last_updated", "coding_complete", "comment", "recommend_qualitative")) %>%
  dplyr::mutate(agree = user_id == user_id1)


all_users<-get_user("all") %>%
  dplyr::select(id, first_name, last_name)
analyse_this <- aa %>%
  dplyr::select(-user_id, -user_id1) %>%
  dplyr::left_join(coder_pairs) %>%
  tidyr::gather(which_user, user_id, user_id, user_id1) %>%
  dplyr::left_join(all_users, by=c("user_id"="id")) %>%
  tidyr::unite(fullname, first_name, last_name)


overall_match <- analyse_this %>%
  dplyr::group_by(variable) %>%
  dplyr::summarize(cases= sum(agree)+sum(!agree), agreement_rate=sum(agree)/(cases)) %>%
  dplyr::mutate(var_f=factor(variable, variable[order(agreement_rate)]))

overall_match %>%
  ggplot(aes(var_f, agreement_rate, size=cases))+
  geom_point()

analyse_this %>%
  dplyr::group_by(variable, fullname) %>%
  dplyr::summarize(cases= sum(agree)+sum(!agree), agreement_rate=sum(agree)/(cases)) %>%
  dplyr::mutate(var_f=factor(variable, overall_match$variable[order(overall_match$agreement_rate)])) %>%
  dplyr::mutate(name_cases=paste0(fullname, " n=", cases)) %>%
  ggplot(aes(var_f, agreement_rate))+
  geom_point(data=overall_match, colour="red")+
  facet_wrap(~name_cases)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f1<-glm(agree ~ variable + name, analyse_this, family="binomial")
f1<-glm(agree ~ variable + name, analyse_this, family="binomial")

stargazer::stargazer(f1, type="text")

bb <- coder_pairs %>%
  tidyr::gather(which_user_id, user_id, user_id, user_id1) %>%
  dplyr::left_join(dplyr::select(all_allocations, user_id, document_id, user_doc_id=id)) %>%
  dplyr::arrange(pair_no) 


get_first<-function(x){
  x[[1]]
}

get_second<-function(x){
  x[[2]]
}

get_third<-function(x){
  x[[3]]
}
cc<-bb %>%
  dplyr::ungroup() %>%
  dplyr::select(-document_id, -user_id) %>%
  tidyr::spread(which_user_id, user_doc_id) %>%
  #dplyr::slice(1:40) %>%
  tidyr::nest(pair_no) %>%
  dplyr::mutate(cal=purrr::map2(user_id, user_id1, get_data_for_comparison)) %>%
  dplyr::mutate(ev_report=purrr::map(cal, get_first), tags=purrr::map(cal, get_second), attributes=purrr::map(cal, get_third))

all_event_reports<-get_event_report()
dd<-cc %>%
  gather(match_type, match_data, ev_report, tags, attributes) %>%
  unnest(data) %>%
  dplyr::select(-cal) %>%
  unnest() %>%
  arrange(pair_no) %>%
  tibble::rowid_to_column("case_no") %>%
  filter(match_type=="ev_report") %>%
  gather(which_one, event_report_id, model_var, user_var) %>%
  left_join(all_event_reports, by=c("event_report_id"="id")) %>%
  dplyr::select(-user_id, -user_id1, -score, -report_type, -process_point, -event_type, -event_id, -comment, -election_date, -summary, -user_doc_id) %>%
  tidyr::gather(variable, value, -pair_no, -case_no, -match_type, -which_one, -event_report_id, -event_end) %>%
  dplyr::select(-event_report_id) %>%
  tidyr::spread(which_one, value) %>%
  dplyr::mutate(agree = model_var==user_var )




er_match <- dd %>%
  dplyr::group_by(variable) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(variable, variable[order(agreement_rate)]))

er_analyse_this <- dd %>%
  dplyr::left_join(coder_pairs) %>%
  tidyr::gather(which_user, user_id, user_id, user_id1) %>%
  dplyr::left_join(all_users, by=c("user_id"="id")) %>%
  tidyr::unite(fullname, first_name, last_name)

er_match %>%
  ggplot(aes(var_f, agreement_rate, size=cases))+
  geom_point()


er_analyse_this %>%
  dplyr::group_by(variable, fullname) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(variable, er_match$variable[order(er_match$agreement_rate)])) %>%
  dplyr::mutate(name_cases=paste0(fullname, " n=", cases)) %>%
  ggplot(aes(var_f, agreement_rate))+
  geom_point(data=er_match, colour="red")+
  facet_wrap(~name_cases)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tags_1<-cc %>%
  gather(match_type, match_data, ev_report, tags, attributes) %>%
  unnest(data) %>%
  dplyr::select(-cal) %>%
  unnest() %>%
  arrange(pair_no) %>%
  tibble::rowid_to_column("case_no") %>%
  filter(match_type=="tags") %>%
  gather(which_one, tag_id, model_var, user_var) %>%
  left_join(all_tags, by=c("tag_id"="id")) %>%
  dplyr::select(-user_id, -user_id1, -score, -comment, -tag_id, -event_report_id) %>%
  tidyr::spread(which_one, tag_value) %>%
  dplyr::mutate(agree = model_var==user_var )

tag_match_overall <- tags_1 %>%
  dplyr::filter(!tag_variable %in% c(NA_character_, "actor", "", "other")) %>%
  dplyr::group_by(tag_variable) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(tag_variable, tag_variable[order(agreement_rate)]))

tags_analyse_this <- tags_1 %>%
  dplyr::left_join(coder_pairs) %>%
  tidyr::gather(which_user, user_id, user_id, user_id1) %>%
  dplyr::left_join(all_users, by=c("user_id"="id")) %>%
  tidyr::unite(fullname, first_name, last_name)

tag_match_overall %>%
  drop_na() %>%
  ggplot(aes(var_f, agreement_rate, size=cases))+
  geom_point()

f1<-glm(agree~fullname  + tag_variable, tags_analyse_this, family="binomial")
stargazer::stargazer(f1, type="text")

tags_analyse_this %>%
  dplyr::group_by(tag_variable, fullname) %>%
  tidyr::drop_na(agree) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  filter(!tag_variable %in% c(NA_character_, "actor", "")) %>%
  dplyr::mutate(var_f=factor(tag_variable, tag_match_overall$tag_variable[order(tag_match_overall$agreement_rate)])) %>%
  ggplot(aes(var_f, agreement_rate))+
  geom_point(data=tag_match_overall, colour="red")+
  facet_wrap(~fullname)+
  geom_point(aes(size=cases))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

attributes_1<-cc %>%
  gather(match_type, match_data, ev_report, tags, attributes) %>%
  unnest(data) %>%
  dplyr::select(-cal) %>%
  unnest() %>%
  arrange(pair_no) %>%
  tibble::rowid_to_column("case_no") %>%
  filter(match_type=="attributes") %>%
  gather(which_one, attribute_id, model_var, user_var) %>%
  left_join(all_attributes, by=c("attribute_id"="id")) %>%
  dplyr::select(-user_id, -user_id1, -score, -attribute_id, -tag_id) %>%
  tidyr::spread(which_one, attribute_value) %>%
  dplyr::mutate(agree = model_var==user_var )

attribute_match_overall <- attributes_1 %>%
  filter(!is.na(attribute), !is.na(agree)) %>%
  dplyr::group_by(attribute) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(attribute, attribute[order(agreement_rate)]))

attributes_analyse_this <- attributes_1 %>%
  dplyr::left_join(coder_pairs) %>%
  tidyr::gather(which_user, user_id, user_id, user_id1) %>%
  dplyr::left_join(all_users, by=c("user_id"="id")) %>%
  tidyr::unite(fullname, first_name, last_name)

attribute_match_overall %>%
  drop_na() %>%
  ggplot(aes(var_f, agreement_rate, size=cases))+
  geom_point()

f1<-glm(agree~fullname  + attribute, attributes_analyse_this, family="binomial")
stargazer::stargazer(f1, type="text")

attributes_analyse_this %>%
  filter(!is.na(attribute), !is.na(agree)) %>%
  dplyr::group_by(attribute, fullname) %>%
  tidyr::drop_na(agree) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(attribute, attribute_match_overall$attribute[order(attribute_match_overall$agreement_rate)])) %>%
  ggplot(aes(var_f, agreement_rate))+
  geom_point(data=attribute_match_overall, colour="red")+
  facet_wrap(~fullname)+
  geom_point(aes(size=cases))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))