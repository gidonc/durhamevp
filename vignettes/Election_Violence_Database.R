## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# devtools::install_github("bergant/datamodelr")
library(datamodelr)

## ---- echo=FALSE, fig.height=7, fig.width=7-----------------------------------


dm <- datamodelr::dm_read_yaml("evp_erd.yml")
graph <- dm_create_graph(dm, view_type = "keys_only")
dm_render_graph(graph)

## ---- echo=FALSE, fig.height=7, fig.width=7-----------------------------------


graph <- dm_create_graph(dm, rankdir="RL", focus=list(tables = c("userdocumentallocation", "event_reports", "tag", "attribute")))
dm_render_graph(graph)

## ---- echo=FALSE, fig.height=7, fig.width=7-----------------------------------


graph <- dm_create_graph(dm, focus=list(tables = c("documents", "candidatedocument", "archivesearchsummaryonly", "archivesearchresult", "archivesearch")), rankdir="RL")
dm_render_graph(graph)

## ---- echo=FALSE--------------------------------------------------------------


graph <- dm_create_graph(dm, focus=list(tables = c("user")))
dm_render_graph(graph)

## ---- echo=FALSE, fig.height=7, fig.width=7-----------------------------------


graph <- dm_create_graph(dm, focus=list(tables = c("userreallocationallocation", "userautodetectedclusterallocation", "verifiedcluster", "verifiedclusterentry", "usercombinationallocation", "combinedclusterentry")))
dm_render_graph(graph)

