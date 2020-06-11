## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(durhamevp)

## ----getarchivesearches, eval=TRUE---------------------------------------

archivesearches<-get_archivesearches()

knitr::kable(tail(archivesearches, 3))


