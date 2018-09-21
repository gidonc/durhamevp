##----readdata----
dropbox.path<-switch (Sys.info()["nodename"],
                      "DM-GIA-051"="D:/Dropbox",
                      "DM-GIA-055"="D:/Dropbox",
                      "GIDON-ASUS-15"="C:/Users/Gidon/Dropbox",
                      "GID-HOME-LENOVO"="C:/Users/Gidon/Dropbox")

project.path<-paste0(dropbox.path, "/ESRC Grant EV 19th Century")
match_data_path<-paste0(project.path, "/Code/database_interactivity/reporting/coder_scoring/data")

long_coding_agreements<-read.csv(paste0(match_data_path, "/long_coding_agreements.csv")) %>%
  dplyr::mutate(match_type=factor(match_type, c("user_doc", "ev_report", "tags", "attributes")))


##----overallagreement----
overall_match <- long_coding_agreements %>%
  drop_na(agree) %>%
  dplyr::group_by(match_type, variable) %>%
  dplyr::summarize(cases= sum(agree)+sum(!agree), agreement_rate=sum(agree)/(cases)) %>%
  ungroup() %>%
  dplyr::mutate(var_f=factor(variable, variable[order(agreement_rate)])) %>%
  dplyr::mutate(match_type=factor(match_type, c("user_doc", "ev_report", "tags", "attributes")))

overall_match %>%
  ggplot(aes(var_f, agreement_rate))+
  facet_wrap(~match_type, scales="free")+
  geom_point(aes(size=cases, colour=match_type)) +
  geom_point()+
  theme_bw()+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##----logsiticsuserdoc----
flogisticuserdoc<-lme4::glmer(agree~variable + (1|fullname), long_coding_agreements[long_coding_agreements$match_type=="user_doc",], family = "binomial")

user_effects<-cbind(lme4::ranef(flogisticuserdoc)$fullname, arm::se.ranef(flogisticuserdoc)$fullname)
names(user_effects)<-c("estimate", "se")
user_effects$fullname<-factor(row.names(user_effects), row.names(user_effects)[order(user_effects$estimate)])

ggplot(user_effects, aes(estimate, fullname))+
  geom_point() +
  geom_errorbarh(aes(xmin=estimate -2*se, xmax=estimate+2*se ))+
  geom_vline(xintercept=0)+
  theme_bw()

##----linearuserdoc----
flinearuserdoc<-lme4::lmer(agree~variable + (1|fullname), long_coding_agreements[long_coding_agreements$match_type=="user_doc",])

user_effects<-cbind(lme4::ranef(flinearuserdoc)$fullname, arm::se.ranef(flinearuserdoc)$fullname)
names(user_effects)<-c("estimate", "se")
user_effects$fullname<-factor(row.names(user_effects), row.names(user_effects)[order(user_effects$estimate)])

ggplot(user_effects, aes(estimate, fullname))+
  geom_point() +
  geom_errorbarh(aes(xmin=estimate -2*se, xmax=estimate+2*se ))+
  geom_vline(xintercept=0)+
  theme_bw()
##----linearall----
flinearall<-lme4::lmer(agree~variable + (1|fullname), long_coding_agreements)

user_effects<-cbind(lme4::ranef(flinearall)$fullname, arm::se.ranef(flinearall)$fullname)
names(user_effects)<-c("estimate", "se")
user_effects$fullname<-factor(row.names(user_effects), row.names(user_effects)[order(user_effects$estimate)])

ggplot(user_effects, aes(estimate, fullname))+
  geom_point() +
  geom_errorbarh(aes(xmin=estimate -2*se, xmax=estimate+2*se ))+
  geom_vline(xintercept=0)+
  theme_bw()

##----agreementsummarybyuser----
agreement_summary <-long_coding_agreements %>%
  dplyr::group_by(match_type, variable, fullname) %>%
  dplyr::summarize(cases= sum(agree, na.rm=TRUE)+sum(!agree, na.rm=TRUE), agreement_rate=sum(agree, na.rm=TRUE)/(cases)) %>%
  dplyr::mutate(var_f=factor(variable, overall_match$variable[order(overall_match$agreement_rate)]))

for (user in unique(agreement_summary$fullname)){
  cat("\n\n")
  cat(paste("###  ", user))
  cat("\n\n" )
  this_data<-dplyr::filter(agreement_summary, fullname==user)
  p1<-this_data%>%
    ggplot(aes(var_f, agreement_rate))+
    facet_wrap(~match_type, scales="free", nrow=1)+
    geom_point()+
    geom_point(data=overall_match, colour="red")+
    theme_bw()+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Agreement by Variable (global average in red)")

  print(p1)
}
