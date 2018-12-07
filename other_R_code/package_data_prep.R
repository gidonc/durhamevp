## ----election_dates----
d.path<-path<-switch (Sys.info()["nodename"],
                      "Vanessas-MacBook-Air.local" = "/Users/vchengm/Dropbox",
                      "DM-GIA-055"="D:/Dropbox/ESRC Grant EV 19th Century",
                      "DM-GIA-051"="D:/Dropbox/ESRC Grant EV 19th Century",
                      "GID-HOME-LENOVO"="C:/Users/Gidon/Dropbox/ESRC Grant EV 19th Century")

election_dates<-readxl::read_excel(paste0(d.path, "/Data Collection/Crawler/election_dates.xlsx"))
devtools::use_data(election_dates, overwrite = TRUE)
