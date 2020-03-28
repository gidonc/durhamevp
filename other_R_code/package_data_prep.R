## ----election_dates----
d.path<-path<-switch (Sys.info()["nodename"],
                      "Vanessas-MacBook-Air.local" = "/Users/vchengm/Dropbox",
                      "DM-GIA-055"="D:/Dropbox/ESRC Grant EV 19th Century",
                      "DM-GIA-051"="D:/Dropbox/ESRC Grant EV 19th Century",
                      "GID-HOME-LENOVO"="C:/Users/Gidon/Dropbox/ESRC Grant EV 19th Century")

election_dates<-readxl::read_excel(paste0(d.path, "/Data Collection/Crawler/election_dates.xlsx"))
election_dates <- dplyr::mutate(election_dates,
                         duration_dissolution_commencement = lubridate::ymd(commencement) - lubridate::ymd(dissolution),
                         polling_duration = lubridate::ymd(polling_end) - lubridate::ymd(polling_start),
                         monthsearch_duration = lubridate::ymd(monthsearch_end) - lubridate::ymd(monthsearch_start))
usethis::use_data(election_dates, overwrite = TRUE)


# Requires the following to be loaded
library(tidyverse)
library(sf)
library(units)

# Requires following paths to be set
d.path<-path<-switch (Sys.info()["nodename"],"Samuels-MacBook-Air.local"="~/Dropbox", "samuels-air.wlan.dur.ac.uk"="~/Dropbox","DM-GIA-055"="D:/Dropbox","DM-GIA-051"="D:/Dropbox", "GID-HOME-LENOVO"="C:/Users/Gidon/Dropbox",
                      "DM-GIA-005"="C:/Users/rjhx32/Dropbox")

evp.path<-paste0(d.path, "/ESRC Grant EV 19th Century")

# define paths to files which we can use throughout code later on
gis.const.path<-paste0(evp.path, "/Data Sources/Constituency Boundary Shape Files")
e85.path<-paste0(gis.const.path, "/E1885_constituencies")
s85.path<-paste0(gis.const.path, "/S1885_constituencies")
w85.path<-paste0(gis.const.path, "/W1885_constituencies")
gis.reg.path <- paste0(evp.path, "/Data Sources/shp_registration_districts")
dist81.path<-paste0(gis.reg.path, "/EW1881_regdistricts")
dist51.path<-paste0(gis.reg.path, "/EW1851_regdistricts")
regions.path<-paste0("/edina_regions")

#1832
e32.path <- paste0(gis.const.path, "/E1832_constituencies")
#s32.path <- paste0(gis.const.path, "/S1832_constituencies")
w32.path <- paste0(gis.const.path, "/W1832_constituencies")
e32 <- st_read(e32.path)
#s32 <- st_read(s32.path)
w32 <- st_read(w32.path)
print(head(e32))
#s32$UNIT_ENDYR <- NA #The dataframes were not the same length - so I have added the extra variable to s32 setting it to NA
c32 <-rbind(e32, w32)

#1862
e62.path <- paste0(gis.const.path, "/E1862_constituencies")
#s32.path <- paste0(gis.const.path, "/S1832_constituencies")
w62.path <- paste0(gis.const.path, "/W1862_constituencies")
e62 <- st_read(e62.path)
#s32 <- st_read(s32.path)
w62 <- st_read(w62.path)
print(head(e62))
#s32$UNIT_STYR <- NA #The dataframes were not the same length - so I have added the extra variable to s32 setting it to NA
c62 <- rbind(e62, w62)

#1868
e68.path <- paste0(gis.const.path, "/E1868_constituencies")
#s68.path <- paste0(gis.path, "/S1868_constituencies")
w68.path <- paste0(gis.const.path, "/W1868_constituencies")
e68 <- st_read(e68.path)
# s68 <- st_read(s68.path)
w68 <- st_read(w68.path)
print(head(e68))
# s68$UNIT_ENDYR <- NA
w68$UNIT_ENDYR <- NA
c68 <- rbind(e68, w68)

#1870
e70.path <- paste0(gis.const.path, "/E1870_constituencies")
# s68.path <- paste0(gis.const.path, "/S1868_constituencies")
w70.path <- paste0(gis.const.path, "/W1870_constituencies")
e70 <- st_read(e70.path)
# s68 <- st_read(s68.path)
w70 <- st_read(w70.path)
print(head(e70))
c70 <- rbind(e70, w70)

#1885
e85.path <- paste0(gis.const.path, "/E1885_constituencies")
# s85.path <- paste0(gis.const.path, "/S1885_constituencies")
w85.path <- paste0(gis.const.path, "/W1885_constituencies")
e85 <- st_read(e85.path)
# s85 <- st_read(s85.path)
w85 <- st_read(w85.path)
print(head(e85))
c85 <- rbind(e85, w85)

# New method - make a single simple features dataframe with all geographies which we will merge at end
#c_all<-rbind(select(c32, g_unit, geometry), select(c62, g_unit, geometry), select(c68, g_unit, geometry), select(c70, g_unit, geometry), select(c85, g_unit, geometry))
in_all<-c("g_unit", "G_NAME", "NAMESTATUS", "UNITTYPE", "G_STATUS", "NATION", "geometry")
election_name_to_boundary_year <- function(x){
  case_when(
    x %in% c("1832", "1835", "1837", "1841", "1847", "1852", "1857", "1859") ~ "1832",
    x %in% c("1865") ~ "1862",
    x %in% c("1868", "1874", "1880") ~ "1868",
    x %in% c("1885", "1886", "1892", "1895", "1900", "1906", "1910_01", "1910_12") ~ "1885"
  )
}

mk_cnty_brgh <- function(x){
  case_when(
    x %in% c("BCon", "PBCon", "BorCon", "BurCon", "DistBCon",  "PBDivCon", "DistBDivCon") ~ "borough",
    x %in% c("PCon", "PDivCon") ~ "county"
  )
}

# now make a single shape file with boundaries for all election years (adding a column to indicate county vs borough measure)
const_shapes_all<-rbind(c32[, in_all] %>% mutate(boundary_year="1832"),
             c62[,in_all]  %>% mutate(boundary_year="1862"),
             c68[,in_all]  %>% mutate(boundary_year="1868"),
             c70[,in_all]  %>% mutate(boundary_year="1870"),
             c85[,in_all]  %>% mutate(boundary_year="1885")) %>%
  mutate(con_type=mk_cnty_brgh(G_STATUS))

const_shape_list <- vector("list")
const_shape_list[["1832"]]<- c32[, in_all] %>% mutate(boundary_year="1832")
const_shape_list[["1862"]]<- c62[, in_all] %>% mutate(boundary_year="1862")
const_shape_list[["1868"]]<- c68[, in_all] %>% mutate(boundary_year="1868")
const_shape_list[["1870"]]<- c70[, in_all] %>% mutate(boundary_year="1870")
const_shape_list[["1885"]]<- c85[, in_all] %>% mutate(boundary_year="1885")

usethis::use_data(const_shape_list, overwrite = TRUE)
usethis::use_data(const_shapes_all, overwrite = TRUE)

cb.path<-paste0(evp.path, "/Data Sources/county_shapefiles")
c91<- paste0(cb.path, "/EW1891_regcounties")
c91 <- st_read(c91)
county_shapes <- c91
usethis::use_data(county_shapes, overwrite = TRUE)
